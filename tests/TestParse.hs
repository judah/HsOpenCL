{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
module Main where

import Parse
import MultiLine
import OpenCL
import OpenCL.Simple
import System.Environment
import Foreign

-- declareKernelsFromFile "prog" "test_prog.cl"
declareKernels "prog" [$clProg| __kernel void
            add(__global float *a,__global float *b, __global float *answer)
            {
	        int gid = get_global_id(0);
	        answer[gid] = a[gid] + b[gid];
            }|]



main = do
    [ns] <- getArgs
    let size = read ns :: Int
    let n = toEnum size
    let context = simpleCxt prog
    let queue = simpleQueue prog
    -- Allocate host memory:
    withArray [0..n-1] $ \a -> do
    withArray [n-1,n-2..0] $ \b -> do
    allocaArray size $ \results -> do
    -- Allocate device memory:
    withBuffer context MemReadOnly NoHostPtr size $ \aMem -> do
    withBuffer context MemReadOnly NoHostPtr size $ \bMem -> do
    withBuffer context MemReadWrite NoHostPtr size $ \ansMem -> do
    -- Run the program:
    waitForCommands queue [aMem =: a, bMem =: b]
    waitForCommand queue $ add size Nothing aMem bMem ansMem
    waitForCommand queue $ results =: ansMem
    putStrLn "First 10 results are:"
    peekArray size results >>= print . take 10
    
    
    
