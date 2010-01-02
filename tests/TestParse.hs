{-# LANGUAGE TemplateHaskell,QuasiQuotes, Rank2Types #-}
module Main where

import OpenCL
import OpenCL.Instances.CArray
import System.Environment
import Foreign

import Data.Array.CArray
import Data.Array.IOCArray
import System.IO

-- declareKernelsFromFile "prog" "test_prog.cl"
declareKernels "ProgAdd" [$clProg| __kernel void
            add(__global float *a,__global float *b, __global float *answer)
            {
	        int gid = get_global_id(0);
	        answer[gid] = a[gid] + b[gid];
            }|]


main = do
    hSetBuffering stdout LineBuffering
    [ns] <- getArgs
    runQueueForType DeviceTypeGPU $ do
    let size = read ns
    let n = toEnum size
    -- Allocate host memory:
    let bounds = (0,size-1)
    let a = asCArray $ listArray bounds [0..n-1]
    b <- asIOCArray $ newListArray bounds [n-1,n-2..0]
    results <- asIOCArray $ newArray_ bounds
    -- Allocate device memory:
    withBuffer MemReadOnly NoHostPtr size $ \aMem -> do
    withBuffer MemReadOnly NoHostPtr size $ \bMem -> do
    withBuffer MemReadWrite NoHostPtr size $ \ansMem -> do
    -- Run the program:
    prog <- buildProgAdd ""
    liftIO $ putStrLn "Running..."
    waitForCommands [aMem =: a, bMem =: b
                    , add prog size Nothing aMem bMem ansMem
                    , results =: ansMem
                    ]
    liftIO $ putStrLn "Done."
    liftIO $ do
        putStrLn "First 10 results are:"
        mapM (readArray results) [0..9] >>= print
        let loopCheck n = if n == size then return ()
                            else do
                                x <- readArray results n
                                if (x/=toEnum (size-1))
                                    then error $ show ("bad entry:",x)
                                    else loopCheck (n+1)
        loopCheck 0


