module OpenCL.Simple(
            SimpleProgram(..),
            DeviceType(..),
            newSimpleProgram,
            getKernelFunc,
            KernelArg(),
            KernelFunc(),
            )
             where

import OpenCL hiding (KernelArg)
import Data.Array.CArray
import Data.Array.IOCArray
import Data.Array.CArray.Base(unsafeFreezeIOCArray, IOCArray(..))
import System.IO
import Control.Exception
import Foreign
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- TODO: this isn't really necessary, since
-- all of the types can ask for their context/id/etc.
data SimpleProgram = SimpleProgram {
                        simpleID :: DeviceID
                        , simpleCxt :: Context
                        , simpleQueue :: CommandQueue
                        , simpleProgram :: Program
                        }

-- TODO: should be bytestring.
newSimpleProgram :: DeviceType -> [ByteString] -> IO SimpleProgram
newSimpleProgram devType sources = do
    devID <- getDeviceID devType
    cxt <- createContext [devID]
    queue <- createCommandQueue cxt devID []
    prog <- buildSimpleProgram devID cxt sources
    return $ SimpleProgram devID cxt queue prog

buildSimpleProgram :: DeviceID -> Context -> [ByteString] -> IO Program
buildSimpleProgram did cxt sources = do
    prog <- createProgramWithSource cxt sources
    handle (\(e::CLError) -> do
                hPutStrLn stderr $ "Error building program: " ++ show e
                log <- getBuildLog prog did
                B.putStrLn log
                throw e)
        $ buildProgram prog ""
    return prog

-- Add note that Doubles probably won't work...
data WrappedArg where
    ReadOnly :: (Ix i, Storable a) => CArray i a -> WrappedArg
    ReadWrite :: (Ix i, Storable a) => IOCArray i a -> WrappedArg
    WriteOnly :: (Ix i, Storable a) => IOCArray i a -> WrappedArg

-- This code inspired by the Translatable class from the llvm package.
class KernelFunc f where
    applyKFunc :: ([WrappedArg] -> IO ())-> Int -> [WrappedArg] -> f

-- TODO: probably more caching opportunities, but whatever for now.
--
-- TODO: better handling of sizes.  type checker should prevent bad
-- TypeFuncs.
-- (also each array should be the same size type...)
-- type func SizeDim f
-- (or, just use Int...)
--
-- Also, (Int -> f) might be ok...
getKernelFunc :: KernelFunc f => SimpleProgram -> String -> IO f
getKernelFunc prog text = do
    kernel <- createKernel (simpleProgram prog) text
    let size = error "Unable to guess size from parameters"
    return $ applyKFunc (runKernel prog kernel) size []

class KernelArg a where
    toArg :: a -> WrappedArg
    argSize :: a -> Int

instance (Ix i, Storable e) => KernelArg (CArray i e) where
    toArg = ReadOnly
    argSize = rangeSize . bounds

instance (Ix i, Storable e) => KernelArg (IOCArray i e) where
    toArg = ReadWrite
    argSize (IOCArray _ _ n _) = n

instance (KernelArg a, KernelFunc f) => KernelFunc (a -> f) where
    applyKFunc run _ as = \a -> applyKFunc run (argSize a) (toArg a:as)

instance KernelFunc (IO ()) where
    applyKFunc run _ as = run $ reverse $ as

instance (Storable e) => KernelFunc (IO (CArray Int e)) where
    applyKFunc run size as = do
        res <- newArray_ (0,size-1)
        run $ reverse $ ReadWrite res : as
        unsafeFreezeIOCArray res

runKernel :: SimpleProgram -> Kernel -> [WrappedArg] -> IO ()
runKernel cxt kernel args = withArgs args $ \argPtrs -> do
    let queue = simpleQueue cxt
    size <- getCommonSize args
    mems <- mapM (bufferArg cxt size) argPtrs
    finish queue
    zipWithM_ (setKernelArg kernel) [0..] mems
    enqueueNDRangeKernel queue kernel [size] Nothing []
    finish queue
    zipWithM_ (copyMutableArg queue size) mems argPtrs
    finish queue
    mapM_ releaseMemObject mems

getCommonSize :: [WrappedArg] -> IO Int
getCommonSize [] = error "No kernel arguments"
getCommonSize ks = do
    (size:sizes) <- mapM getArgSize ks
    if any (/=size) sizes
        then error "Kernel arguments are not all the same size"
        else return size

getArgSize :: WrappedArg -> IO Int
getArgSize (ReadOnly a) = return $ rangeSize $ bounds a
getArgSize (ReadWrite a) = rangeSize <$> getBounds a
getArgSize (WriteOnly a) = rangeSize <$> getBounds a

data KernelPtrArg where
    ReadOnlyPtr :: Storable a =>  Ptr a -> KernelPtrArg
    ReadWritePtr :: Storable a => Ptr a -> KernelPtrArg
    WriteOnlyPtr :: Storable a => Ptr a -> KernelPtrArg

-- make sure we retain the ForeignPtrs for the whole computation:
withArgs :: [WrappedArg] -> ([KernelPtrArg] -> IO a) -> IO a
withArgs [] f = f []
withArgs (ReadOnly x:xs) f = withCArray x $ \p -> withArgs xs 
                                $ \ys -> f (ReadOnlyPtr p:ys)
withArgs (ReadWrite x:xs) f = withIOCArray x $ \p -> withArgs xs
                                $ \ys -> f (ReadWritePtr p:ys)
withArgs (WriteOnly x:xs) f = withIOCArray x $ \p -> withArgs xs
                                $ \ys -> f (WriteOnlyPtr p:ys)

-- Being lazy (since I'll end up rewriting this anyway...)
-- and casting everything to CLMem ().

-- TODO: be more efficient
-- We have to be careful since we don't want it to be freed
bufferArg :: SimpleProgram -> Int -> KernelPtrArg -> IO (Buffer ())
bufferArg cxt size (ReadOnlyPtr p) = do
    mem <- createBuffer (simpleCxt cxt) MemReadOnly NoHostPtr size
    enqueueWriteBuffer (simpleQueue cxt) mem NonBlocking 0 size p []
    return $ castBuffer mem
bufferArg cxt size (ReadWritePtr p) = do
    mem <- createBuffer (simpleCxt cxt) MemReadWrite NoHostPtr size
    enqueueWriteBuffer (simpleQueue cxt) mem NonBlocking 0 size p []
    return $ castBuffer mem
bufferArg cxt size (WriteOnlyPtr (p::Ptr a)) = fmap castBuffer
    (createBuffer (simpleCxt cxt) MemWriteOnly NoHostPtr size
        :: IO (Buffer a))


copyMutableArg :: CommandQueue -> Int -> Buffer () -> KernelPtrArg -> IO ()
copyMutableArg _ _ _ (ReadOnlyPtr _) = return ()
copyMutableArg queue size mem (WriteOnlyPtr p) =
    enqueueReadBuffer queue (castBuffer mem) NonBlocking 0 size p []
        >> return ()
copyMutableArg queue size mem (ReadWritePtr p) =
    enqueueReadBuffer queue (castBuffer mem) NonBlocking 0 size p []
        >> return ()
     


