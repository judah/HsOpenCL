{-# LANGUAGE ScopedTypeVariables, QuasiQuotes#-}
module Main where

import OpenCL
import OpenCL.Instances.CArray

import qualified Data.ByteString as B
import Data.Array.CArray
import Data.Array.IOCArray

import System.Environment

myprog = [$clProg|
            __kernel void add(__global float *a, __global float *b,
                                float c, __global float *answer)
            {
                int gid = get_global_id(0);
                answer[gid] = a[gid] + c*b[gid];
            }|]

main = runQueueForType DeviceTypeGPU $ do
    setProperties [QueueProfilingEnable] True
    [n] <- liftIO $ getArgs
    prog <- buildProgramFromSource "" [myprog]
    kernel <- liftIO $ createKernel prog "add"
    -- Allocate the host memory:
    let size = read n :: Int
    let n = toEnum size
    let bounds = (0,size-1)
    let a :: CArray Int Float = listArray bounds [0..n-1]
    b :: IOCArray Int Float <- newListArray bounds [n-1,n-2..0]
    results :: IOCArray Int Float <- newArray_ bounds
    -- Allocate the device memory, and copy the data manually:
    allocaBuffer MemReadOnly NoHostPtr size $ \aMem -> do
    allocaBuffer MemReadOnly NoHostPtr size $ \bMem -> do
    allocaBuffer MemReadWrite NoHostPtr size $ \ansMem -> do
    waitForCommands [aMem =: a, bMem =: b]
    -- Run the kernel:
    eKernel <- waitForCommand
                $ runKernel kernel size Nothing
                                aMem bMem (2::Float) ansMem
    -- Print out the results:
    waitForCommand (results =: ansMem)
    liftIO $ do
        statEvent eKernel
        putStrLn "First 10 results are:"
        mapM (readArray results) [0..9] >>= print

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
    
