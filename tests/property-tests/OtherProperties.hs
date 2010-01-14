{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foreign
import System.HsOpenCL
import Control.Exception

main = do
    cxt <- createContextFromType DeviceTypeAll
    let devs = contextDevices cxt
    print devs
    print (map deviceType devs)
    queue <- createCommandQueue cxt (head devs) [QueueProfilingEnable]
    testQueueInfos queue
    runQueueT testBufferInfos queue

testQueueInfos q = do
    putStrLn "--- CLCommandQueue ---"
    print (queueDevice q)
    print (contextDevices (queueContext q))
    getQueueProperties q >>= print
    handle (\(e::SomeException) -> putStrLn $ "Got exception: " ++ show e)
        $ setQueueProperties q [QueueOutOfOrderExecModeEnable] False
    getQueueProperties q >>= print
    setQueueProperties q [QueueProfilingEnable] False
    getQueueProperties q >>= print

testBufferInfos = do
    p :: Ptr Float <- liftIO $ newArray [1..10]
    buf :: Buffer Float <- newBuffer MemReadOnly (CopyAllocHostPtr p) 5
    buf2 :: Buffer Float <- newBuffer MemReadWrite (UseHostPtr p) 5
    liftIO $ do
    putStrLn "--- Buffer ---"
    print p
    putStrLn "Flags:"
    print $ memFlags buf
    print $ memSize buf
    print $ memFlags buf2
