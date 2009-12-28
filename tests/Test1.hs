{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import OpenCL

import Foreign
import Foreign.C
import qualified Data.ByteString as B

import System.Environment

import Control.Exception

main = do
    [n] <- getArgs
    let file = "test_prog.cl"
    contents <- B.readFile file
    dev <- getDeviceID DeviceTypeGPU
    print ("device:",dev)
    context <- createContext [dev]
    queue <- createCommandQueue context dev []
    prog <- createProgramWithSource context [contents]
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
    aMem <- createBuffer context CLMemReadOnly NoHostPtr size
    enqueueWriteBuffer queue aMem size a
    bMem <- createBuffer context CLMemReadOnly NoHostPtr size
    enqueueWriteBuffer queue bMem size b
    ansMem <- createBuffer context CLMemReadWrite NoHostPtr size
    putStrLn "Allocated buffers."
    finish queue
    putStrLn "Finished copying to buffers."
    -- Kernel arguments
    setKernelMemArg kernel 0 aMem
    setKernelMemArg kernel 1 bMem
    setKernelMemArg kernel 2 ansMem
    putStrLn "Args set."
    enqueueNDRangeKernel queue kernel [size]
    putStrLn "Running..."
    finish queue
    putStrLn "Finished running!"
    enqueueReadBuffer queue ansMem size results
    finish queue
    mapM_ clReleaseMemObject [aMem,bMem,ansMem]
    putStrLn "Results are:"
    peekArray size results >>= print . take 10


