module OpenCL.Simple(
            SimpleProgram(..),
            DeviceType(..),
            newSimpleProgram,
            CLKernel,
            getKernel,
            KernelArg(..),
            runKernel
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

-- TODO: this isn't really necessary, since
-- all of the types can ask for their context/id/etc.
data SimpleProgram = SimpleProgram {
                        simpleID :: DeviceID
                        , simpleCxt :: CLContext
                        , simpleQueue :: CLCommandQueue
                        , simpleProgram :: CLProgram
                        }

-- TODO: should be bytestring.
newSimpleProgram :: DeviceType -> [ByteString] -> IO SimpleProgram
newSimpleProgram devType sources = do
    devID <- getDeviceID devType
    cxt <- createContext [devID]
    queue <- createCommandQueue cxt devID []
    prog <- buildSimpleProgram devID cxt sources
    return $ SimpleProgram devID cxt queue prog

buildSimpleProgram :: DeviceID -> CLContext -> [ByteString] -> IO CLProgram
buildSimpleProgram did cxt sources = do
    prog <- createProgramWithSource cxt sources
    handle (\(e::CLError) -> do
                hPutStrLn stderr $ "Error building program: " ++ show e
                log <- getBuildLog prog did
                putStrLn log
                throw e)
        $ buildProgram prog
    return prog


{-
data KernelArgType = ReadOnly | ReadWrite | WriteOnly

data SomeStorable a where
    SomeStorable :: Storable e => a e -> SomeStorable a
class KernelResult a where
    resultArrays :: a -> [SomeStorable (CArray Int)]

instance Storable a => KernelResult (CArray Int a) where
    resultArrays 

OK, question becomes whether we want to set the buf size each time...


OK, API is:
data KernelFunc f = KernelFunc {funcKernel :: Kernel,
                                kernelArgs :: KernelArgs
                                numKernelReturns :: Int
                                }
getKernel :: KernelFuncType f => Program -> String -> IO KernelFun f

class KernelFuncType f where
    

-}
getKernel :: SimpleProgram -> String -> IO CLKernel
getKernel = clCreateKernel . simpleProgram

-- Add note that Doubles probably won't work...
data KernelArg where
    ReadOnly :: Storable a => CArray Int a -> KernelArg
    ReadWrite :: Storable a => IOCArray Int a -> KernelArg
    WriteOnly :: Storable a => IOCArray Int a -> KernelArg
    -- TODO: just a pure output...

-- TODO: should kernel contain ref to context?

-- TODO: Multi-dimensional

-- TODO: some sort of class structure so we can make pure functions...
-- TODO: Use that so we don't need to create new buffers each time...
-- And don't need to check sizes each time, either.
--
-- Really what we want is
-- class KernelFunc f where
--
-- instance Storable a => KernelFunc (CArray Int a)
-- instance Storable a => KernelFunc (CArray Int a, CArray Int a)
-- etc.
-- instance KernelFunc f, Storable a => KernelFunc (CArray Int a -> f)
-- instance KernelFunc f, Storable a => KernelFunc (IOCArray Int a -> f) 
--
-- And then of course deal with 2d and 3d data...
runKernel :: SimpleProgram -> CLKernel -> [KernelArg] -> IO ()
runKernel cxt kernel args = withArgs args $ \argPtrs -> do
    let queue = simpleQueue cxt
    size <- getCommonSize args
    mems <- mapM (bufferArg cxt size) argPtrs
    clFinish queue
    zipWithM_ (setKernelMemArg kernel) [0..] mems
    enqueueNDRangeKernel queue kernel [size]
    clFinish queue
    zipWithM_ (copyMutableArg queue size) mems argPtrs
    clFinish queue
    mapM_ clReleaseMemObject mems

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
bufferArg :: SimpleProgram -> Int -> KernelPtrArg -> IO (CLMem ())
bufferArg cxt size (ReadOnlyPtr p) = do
    mem <- createBuffer (simpleCxt cxt) CLMemReadOnly NoHostPtr size
    enqueueWriteBuffer (simpleQueue cxt) mem size p
    return $ castCLMem mem
bufferArg cxt size (ReadWritePtr p) = do
    mem <- createBuffer (simpleCxt cxt) CLMemReadWrite NoHostPtr size
    enqueueWriteBuffer (simpleQueue cxt) mem size p
    return $ castCLMem mem
bufferArg cxt size (WriteOnlyPtr (p::Ptr a)) = fmap castCLMem
    (createBuffer (simpleCxt cxt) CLMemWriteOnly NoHostPtr size
        :: IO (CLMem a))


copyMutableArg :: CLCommandQueue -> Int -> CLMem () -> KernelPtrArg -> IO ()
copyMutableArg _ _ _ (ReadOnlyPtr _) = return ()
copyMutableArg queue size mem (WriteOnlyPtr p) =
    enqueueReadBuffer queue (castCLMem mem) size p
copyMutableArg queue size mem (ReadWritePtr p) =
    enqueueReadBuffer queue (castCLMem mem) size p
     


