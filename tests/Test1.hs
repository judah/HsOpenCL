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
__kernel void
add(__global float *a,
    __global float *b,
    float c,
    __global float *answer)
{
    int gid = get_global_id(0);
    answer[gid] = a[gid] + c*b[gid];
}
|]

main = do
    [n] <- getArgs
    let file = "test_prog.cl"
    dev <- getDeviceID DeviceTypeGPU
    print ("device:",dev)
    context <- createContext [dev]
    queue <- createCommandQueue context dev [QueueProfilingEnable]
    prog <- createProgramWithSource context [myprog]
    print "Created!"
    handle (\(e::CLError) -> do
                print ("exception:",e)
                log <- getBuildLog prog dev
                print log
                throw e)
            $ buildProgram prog ""
    print "Built!"
    kernel <- createKernel prog "add"
    print "Kernel!"
    -- Allocate the buffers...
    let size = read n :: Int
    let n = toEnum size
    a :: Ptr Float <- newArray [0..n-1]
    b :: Ptr Float <- newArray [n-1,n-2..0]
    results :: Ptr Float <- newArray $ replicate size 0
    aMem <- createBuffer context MemReadOnly NoHostPtr size
    enqueueWriteBuffer queue aMem NonBlocking 0 size a []
    bMem <- createBuffer context MemReadOnly NoHostPtr size
    enqueueWriteBuffer queue bMem NonBlocking 0 size b []
    ansMem <- createBuffer context MemReadWrite NoHostPtr size
    putStrLn "Allocated buffers."
    finish queue
    putStrLn "Finished copying to buffers."
    -- Kernel arguments
    setKernelArgs kernel $ aMem :& bMem :& Scalar (5::Float)
                                :& ansMem :& KNil
    putStrLn "Args set."
    eKernel <- enqueueNDRangeKernel queue kernel [size] Nothing []
    putStrLn "Running..."
    finish queue
    putStrLn "Finished running!"
    statEvent "enqueueNDRangeKernel" eKernel
    enqueueReadBuffer queue ansMem NonBlocking 0 size results []
    finish queue
    mapM_ releaseMemObject [aMem,bMem,ansMem]
    putStrLn "Results are:"
    peekArray size results >>= print . take 10

statEvent text e = do
    putStrLn $ "--- Status: " ++ text ++ " --- "
    putStr "Queued: " >> getCommandQueued e >>= print
    putStr "Submit: " >> getCommandSubmit e >>= print
    start <- getCommandStart e
    end <- getCommandEnd e
    putStrLn $ "Start: " ++ show start
    putStrLn $ "End: " ++ show end
    putStrLn $ "Duration: " ++ show (end-start)
    putStrLn ""
    
