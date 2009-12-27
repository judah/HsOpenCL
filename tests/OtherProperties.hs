{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foreign
import OpenCL

main = do
    cxt <- createContextFromType DeviceTypeAll
    let devs = contextDevices cxt
    print devs
    print (map deviceType devs)
    queue <- createCommandQueue cxt (head devs) [QueueProfilingEnable]
    testQueueInfos queue
    testBufferInfos cxt

testQueueInfos q = do
    putStrLn "--- CLCommandQueue ---"
    print (queueDevice q)
    print (contextDevices (queueContext q))
    getQueueProperties q >>= print
    setQueueProperties q [QueueOutOfOrderExecModeEnable] False
    getQueueProperties q >>= print
    setQueueProperties q [QueueProfilingEnable] False
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
