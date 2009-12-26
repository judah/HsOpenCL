module OpenCL.Simple(
            SimpleContext,
            CLDeviceType(..),
            newSimpleContext,
            CLProgram,
            buildSimpleProgram,
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

data SimpleContext = SimpleContext {
                        simpleID :: CLDeviceID
                        , simpleCxt :: CLContext
                        , simpleQueue :: CLCommandQueue
                        }

newSimpleContext :: CLDeviceType -> IO SimpleContext
newSimpleContext devType = do
    devID <- getDeviceID devType
    cxt <- createContext devID
    queue <- clCreateCommandQueue cxt devID []
    return $ SimpleContext devID cxt queue

buildSimpleProgram :: SimpleContext -> [String] -> IO CLProgram
buildSimpleProgram cxt contents = do
    prog <- createProgramWithSource (simpleCxt cxt) contents
    handle (\(e::CLError) -> do
                hPutStrLn stderr $ "Error building program: " ++ show e
                log <- getBuildLog prog (simpleID cxt)
                putStrLn log
                throw e)
        $ buildProgram prog
    return prog

getKernel :: CLProgram -> String -> IO CLKernel
getKernel = clCreateKernel

-- Add note that Doubles probably won't work...
data KernelArg where
    Immutable :: Storable a => CArray Int a -> KernelArg
    Mutable :: Storable a => IOCArray Int a -> KernelArg
    -- TODO: just a pure output...

-- TODO: should kernel contain ref to context?

-- TODO: Multi-dimensional

-- TODO: some sort of class structure so we can make pure functions...
-- TODO: Use that so we don't need to create new buffers each time...
-- And don't need to check sizes each time, either.
runKernel :: SimpleContext -> CLKernel -> [KernelArg] -> IO ()
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

getCommonSize :: [KernelArg] -> IO Int
getCommonSize [] = error "No kernel arguments"
getCommonSize ks = do
    (size:sizes) <- mapM getArgSize ks
    if any (/=size) sizes
        then error "Kernel arguments are not all the same size"
        else return size
  where
    getArgSize :: KernelArg -> IO Int
    getArgSize (Immutable a) = return $ rangeSize $ bounds a
    getArgSize (Mutable a) = rangeSize <$> getBounds a

data KernelPtrArg where
    ImmutablePtr :: Storable a =>  Ptr a -> KernelPtrArg
    MutablePtr :: Storable a => Ptr a -> KernelPtrArg

-- make sure we retain the ForeignPtrs for the whole computation:
withArgs :: [KernelArg] -> ([KernelPtrArg] -> IO a) -> IO a
withArgs [] f = f []
withArgs (Immutable x:xs) f = withCArray x $ \p -> withArgs xs 
                                $ \ys -> f (ImmutablePtr p:ys)
withArgs (Mutable x:xs) f = withIOCArray x $ \p -> withArgs xs
                                $ \ys -> f (MutablePtr p:ys)

-- TODO: be more efficient
-- We have to be careful since we don't want it to be freed
bufferArg :: SimpleContext -> Int -> KernelPtrArg -> IO CLMem
bufferArg cxt size (ImmutablePtr p) = do
    mem <- createBuffer (simpleCxt cxt) [CLMemReadOnly] size p
    enqueueWriteBuffer (simpleQueue cxt) mem size p
    return mem
bufferArg cxt size (MutablePtr p) = do
    mem <- createBuffer (simpleCxt cxt) [CLMemReadWrite] size p
    enqueueWriteBuffer (simpleQueue cxt) mem size p
    return mem


copyMutableArg :: CLCommandQueue -> Int -> CLMem -> KernelPtrArg -> IO ()
copyMutableArg _ _ _ (ImmutablePtr _) = return ()
copyMutableArg queue size mem (MutablePtr p) =
    enqueueReadBuffer queue mem size p
     


