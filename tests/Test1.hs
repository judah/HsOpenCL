{-# LANGUAGE ScopedTypeVariables, QuasiQuotes#-}
module Main where

import OpenCL
import MultiLine

import Foreign
import Foreign.C
import qualified Data.ByteString as B

import System.Environment

import Control.Exception

myprog = [$clProg|
            __kernel void add(__global float *a, __global float *b,
                                float c, __global float *answer)
            {
                int gid = get_global_id(0);
                answer[gid] = a[gid] + c*b[gid];
            }|]

main = do
    [n] <- getArgs
    -- Initialize the context:
    dev <- getDeviceID DeviceTypeGPU
    print ("device:",dev)
    context <- createContext [dev]
    queue <- createCommandQueue context dev [QueueProfilingEnable]
    prog <- createProgramWithSource context [myprog]
    -- Build the program:
    handle (\(e::CLError) -> do
                print ("exception:",e)
                log <- getBuildLog prog dev
                print log
                throw e)
            $ buildProgram prog ""
    kernel <- createKernel prog "add"
    -- Allocate the host memory:
    let size = read n :: Int
    let n = toEnum size
    withArray [0..n-1] $ \(a :: Ptr Float) -> do
    withArray [n-1,n-2..0] $ \(b :: Ptr Float) -> do
    allocaArray size $ \(results :: Ptr Float) -> do
    -- Allocate the device memory, and copy the data manually:
    withBuffer context MemReadOnly NoHostPtr size $ \aMem -> do
    withBuffer context MemReadOnly NoHostPtr size $ \bMem -> do
    withBuffer context MemReadWrite NoHostPtr size $ \ansMem -> do
    waitForCommands queue [writeBuffer aMem NonBlocking 0 size a
                          ,writeBuffer bMem NonBlocking 0 size b
                          ]
    -- Run the kernel:
    setKernelArgs kernel $ aMem &: bMem &: Scalar (2::Float)
                                &: ansMem &: []
    eKernel <- enqueue queue (ndRangeKernel kernel size Nothing) []
    waitForEvent eKernel
    statEvent eKernel
    -- Get the result, and print it out:
    waitForCommand queue (readBuffer ansMem NonBlocking 0 size results)
    putStrLn "First 10 results are:"
    peekArray size results >>= print . take 10

statEvent e = do
    putStrLn "Kernel timings:"
    putStr "Queued: " >> getCommandQueued e >>= print
    putStr "Submit: " >> getCommandSubmit e >>= print
    start <- getCommandStart e
    end <- getCommandEnd e
    putStrLn $ "Start: " ++ show start
    putStrLn $ "End: " ++ show end
    putStrLn $ "Duration: " ++ show (end-start)
    putStrLn ""
    
