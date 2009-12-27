{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foreign
import OpenCL

main = do
    cxt <- createContextFromType DeviceTypeAll
    let devs = clContextDevices cxt
    print devs
    print (map clDeviceType devs)
    queue <- createCommandQueue cxt (head devs) [CLQueueProfilingEnable]
    testQueueInfos queue
    testBufferInfos cxt

testQueueInfos q = do
    putStrLn "--- CLCommandQueue ---"
    print (clQueueDevice q)
    print (clContextDevices (clQueueContext q))
    getQueueProperties q >>= print
    setQueueProperties q [CLQueueOutOfOrderExecModeEnable] False
    getQueueProperties q >>= print
    setQueueProperties q [CLQueueProfilingEnable] False
    getQueueProperties q >>= print

testBufferInfos cxt = do
    putStrLn "--- CLMem ---"
    p :: Ptr Float <- newArray [1..10]
    print p
    buf :: CLMem Float <- createBuffer cxt CLMemReadOnly (CopyAllocHostPtr p) 5
    clGetMemReferenceCount buf >>= print
    clRetainMemObject buf
    clGetMemReferenceCount buf >>= print
    putStrLn "Flags:"
    print $ clMemFlags buf
    print $ clMemSize buf
    buf2 :: CLMem Float <- createBuffer cxt CLMemReadWrite (UseHostPtr p) 5
    print $ clMemFlags buf
