module OpenCL.Simple(
            SimpleProgram(..),
            DeviceType(..),
            newSimpleProgram,
            KernelFunc(),
            getKernelFunc,
            )
             where

import OpenCL
import Data.Array.CArray
import Data.Array.IOCArray
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
data KernelArg where
    ReadOnly :: Storable a => CArray Int a -> KernelArg
    ReadWrite :: Storable a => IOCArray Int a -> KernelArg
    WriteOnly :: Storable a => IOCArray Int a -> KernelArg

-- This code inspired by the Translatable class from the llvm package.
class KernelFunc f where
    applyKFunc :: ([KernelArg] -> IO ()) -> [KernelArg] -> f

-- TODO: probably more caching opportunities, but whatever for now.
getKernelFunc :: KernelFunc f => SimpleProgram -> String -> IO f
getKernelFunc prog text = do
    kernel <- createKernel (simpleProgram prog) text
    return $ applyKFunc (runKernel prog kernel) []

instance KernelFunc (IO ()) where
    applyKFunc run args = run $ reverse args

instance (Storable a, KernelFunc f) => KernelFunc (CArray Int a -> f) where
    applyKFunc run as = \a -> applyKFunc run (ReadOnly a:as)

instance (Storable a, KernelFunc f) => KernelFunc (IOCArray Int a -> f) where
    applyKFunc run as = \a -> applyKFunc run (WriteOnly a:as)

runKernel :: SimpleProgram -> Kernel -> [KernelArg] -> IO ()
runKernel cxt kernel args = withArgs args $ \argPtrs -> do
    let queue = simpleQueue cxt
    size <- getCommonSize args
    mems <- mapM (bufferArg cxt size) argPtrs
    finish queue
    zipWithM_ (setKernelMemArg kernel) [0..] mems
    enqueueNDRangeKernel queue kernel [size] []
    finish queue
    zipWithM_ (copyMutableArg queue size) mems argPtrs
    finish queue
    mapM_ releaseMemObject mems

getCommonSize :: [KernelArg] -> IO Int
getCommonSize [] = error "No kernel arguments"
getCommonSize ks = do
    (size:sizes) <- mapM getArgSize ks
    if any (/=size) sizes
        then error "Kernel arguments are not all the same size"
        else return size
  where
    getArgSize :: KernelArg -> IO Int
    getArgSize (ReadOnly a) = return $ rangeSize $ bounds a
    getArgSize (ReadWrite a) = rangeSize <$> getBounds a
    getArgSize (WriteOnly a) = rangeSize <$> getBounds a

data KernelPtrArg where
    ReadOnlyPtr :: Storable a =>  Ptr a -> KernelPtrArg
    ReadWritePtr :: Storable a => Ptr a -> KernelPtrArg
    WriteOnlyPtr :: Storable a => Ptr a -> KernelPtrArg

-- make sure we retain the ForeignPtrs for the whole computation:
withArgs :: [KernelArg] -> ([KernelPtrArg] -> IO a) -> IO a
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
     


