{-# LANGUAGE ScopedTypeVariables, QuasiQuotes#-}
module Main where

import System.HsOpenCL
import System.HsOpenCL.Instances.CArray

import qualified Data.ByteString as B
import Data.Array.CArray
import Data.Array.IOCArray

import System.Environment
import Control.Monad
import Data.Word
import Data.List
import Text.Printf

myprog = [clProg|
            __kernel void add(__global float *a, __global float *b,
                                float c, __global float *answer)
            {
                int gid = get_global_id(0);
                answer[gid] = a[gid] + c*b[gid];
            }|]

main = runQueueForType DeviceTypeGPU $ do
    setProperties [QueueProfilingEnable] True
    [n] <- liftIO $ getArgs
    prog <- buildProgramFromSource "" [myprog]
    kernel <- liftIO $ createKernel prog "add"
    -- Allocate the host memory:
    let size = read n :: Int
    let n = toEnum size
    let bounds = (0,size-1)
    let a :: CArray Int Float = listArray bounds [0..n-1]
    b :: IOCArray Int Float <- newListArray bounds [n-1,n-2..0]
    results :: IOCArray Int Float <- newArray_ bounds
    -- Allocate the device memory, and copy the data manually:
    allocaBuffer MemReadOnly NoHostPtr size $ \aMem -> do
    allocaBuffer MemReadOnly NoHostPtr size $ \bMem -> do
    allocaBuffer MemReadWrite NoHostPtr size $ \ansMem -> do
    waitForCommands [aMem =: a, bMem =: b]
    -- Run the kernel:
    let numTests = 50
    eKernels <- waitForCommands $ replicate numTests
                $ runKernel kernel size Nothing
                                aMem bMem (2::Float) ansMem
    times <- liftIO $ forM eKernels $ \e -> liftM2 (-) (getCommandEnd e) (getCommandStart e)
    -- Print out the results:
    waitForCommand (results =: ansMem)
    liftIO $ do
        putStrLn "First 10 results are:"
        mapM (readArray results) [0..9] >>= print
        let ave = foldl1' (+) (map cvtTime times) / toEnum numTests
        printf "Average kernel run time: %.5f seconds\n" ave

cvtTime :: Word64 -> Double
cvtTime w = fromIntegral w / 10^9
