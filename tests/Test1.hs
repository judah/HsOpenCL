{-# LANGUAGE ScopedTypeVariables #-}
module Test1 where

import OpenCL.Platform
import OpenCL.Context hiding (clCreateCommandQueue)
import OpenCL.Error
import OpenCL.Kernel
import OpenCL.Buffer
import OpenCL.CommandQueue

import Foreign
import Foreign.C

import Control.Exception
showDevice dev = clDeviceVendor dev >>= print 
                >> clDeviceName dev >>= print

test1 = do
    getDeviceID DeviceTypeCPU >>= showDevice
    getDeviceID DeviceTypeGPU >>= showDevice

test2 file = do
    contents <- readFile file
    dev <- getDeviceID DeviceTypeGPU
    context <- clCreateContext dev
    queue <- clCreateCommandQueue context dev []
    prog <- createProgramWithSource context [contents]
    print "Created!"
    handle (\(e::CLError) -> do
                print ("exception:",e)
                log <- getBuildLog prog dev
                print log
                throw e)
            $ buildProgram prog
    print "Built!"
    kernel <- clCreateKernel prog "add"
    print "Kernel!"
    -- Allocate the buffers...
    let size = 32 :: Int
    let n = toEnum size
    a :: Ptr Double <- newArray [0..n-1]
    b :: Ptr Double <- newArray [n-1,n-2..0::Double]
    results :: Ptr Double <- newArray $ replicate size 0
    aMem <- createBuffer context [CLMemReadOnly] size a
    enqueueWriteBuffer queue aMem size a
    bMem <- createBuffer context [CLMemReadOnly] size b
    enqueueWriteBuffer queue bMem size b
    -- result:
    ansMem <- createBuffer context [CLMemReadWrite] size a
    clFinish queue
    putStrLn "Finished copying to buffers."
    -- Test
    enqueueReadBuffer queue aMem size results
    peekArray size results >>= print
    enqueueReadBuffer queue bMem size results
    peekArray size results >>= print
    

    -- Kernel arguments
    setKernelMemArg kernel 0 aMem
    setKernelMemArg kernel 1 bMem
    setKernelMemArg kernel 2 ansMem
    putStrLn "Args set."
    -- enqueueNDRangeKernel queue kernel [size]
    with (toEnum size) $ \globalWorkSize -> do
        clEnqueueNDRangeKernel queue kernel 1 nullPtr globalWorkSize nullPtr 0
                                    nullPtr nullPtr
        putStrLn "Running..."
        clFinish queue
    putStrLn "Finished running!"
    enqueueReadBuffer queue ansMem size results
    clFinish queue
    putStrLn "Results are:"
    peekArray size results >>= print


