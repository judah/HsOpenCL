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
    putStrLn "--- Buffer ---"
    p :: Ptr Float <- newArray [1..10]
    print p
    buf :: Buffer Float <- createBuffer cxt MemReadOnly (CopyAllocHostPtr p) 5
    getMemReferenceCount buf >>= print
    retainMemObject buf
    getMemReferenceCount buf >>= print
    putStrLn "Flags:"
    print $ memFlags buf
    print $ memSize buf
    buf2 :: Buffer Float <- createBuffer cxt MemReadWrite (UseHostPtr p) 5
    print $ memFlags buf
