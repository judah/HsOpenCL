{-# LANGUAGE TemplateHaskell,QuasiQuotes, Rank2Types #-}
module Main where

import Parse
import OpenCL
import System.Environment
import Foreign

import OpenCL.Instances.CArray
import Data.Array.CArray
import Data.Array.IOCArray

-- declareKernelsFromFile "prog" "test_prog.cl"
declareKernels "ProgAdd" [$clProg| __kernel void
            add(__global float *a,__global float *b, __global float *answer)
            {
	        int gid = get_global_id(0);
	        answer[gid] = a[gid] + b[gid];
            }|]


main = runQueueForType DeviceTypeGPU $ do
    [ns] <- liftIO getArgs
    let size = read ns
    let n = toEnum size
    -- Allocate host memory:
    let bounds = (0,size-1)
    let a = asCArray $ listArray bounds [0..n-1]
    b <- asIOCArray $ liftIO $ newListArray bounds [n-1,n-2..0]
    results <- asIOCArray $ liftIO $ newArray_ bounds
    -- Allocate device memory:
    withBuffer MemReadOnly NoHostPtr size $ \aMem -> do
    withBuffer MemReadOnly NoHostPtr size $ \bMem -> do
    withBuffer MemReadWrite NoHostPtr size $ \ansMem -> do
    -- Run the program:
    prog <- buildProgAdd
    waitForCommands [aMem =: a, bMem =: b]
    waitForCommands [add prog size Nothing aMem bMem ansMem]
    waitForCommands [results =: ansMem]
    liftIO $ do
        putStrLn "First 10 results are:"
        mapM (readArray results) [0..9] >>= print
