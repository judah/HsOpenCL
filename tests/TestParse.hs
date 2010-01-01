{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
module Main where

import Parse
import MultiLine
import OpenCL
import OpenCL.Simple
import System.Environment
import Foreign

import OpenCL.Instances.CArray
import Data.Array.CArray
import Data.Array.IOCArray

-- declareKernelsFromFile "prog" "test_prog.cl"
declareKernels "prog" [$clProg| __kernel void
            add(__global float *a,__global float *b, __global float *answer)
            {
	        int gid = get_global_id(0);
	        answer[gid] = a[gid] + b[gid];
            }|]



main = do
    [ns] <- getArgs
    let size = read ns
    let n = toEnum size
    let context = simpleCxt prog
    let queue = simpleQueue prog
    -- Allocate host memory:
    let bounds = (0,size-1)
    let a = asCArray $ listArray bounds [0..n-1]
    b <- asIOCArray $ newListArray bounds [n-1,n-2..0]
    results <- asIOCArray $ newArray_ bounds
    -- Allocate device memory:
    withBuffer context MemReadOnly NoHostPtr size $ \aMem -> do
    withBuffer context MemReadOnly NoHostPtr size $ \bMem -> do
    withBuffer context MemReadWrite NoHostPtr size $ \ansMem -> do
    -- Run the program:
    -- Since we're not running out of order, we can queue all of
    -- these up at once:
    waitForCommands queue [aMem =: a, bMem =: b
            , add size Nothing aMem bMem ansMem
            , results =: ansMem
            ]
    putStrLn "First 10 results are:"
    getElems results >>= print . take 10
