{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.HsOpenCL

import Foreign
import qualified Data.ByteString.Char8 as B

n = 16

main = do
    file <- B.readFile "test_prog.cl"
    withArray [0..toEnum n-1] $ \(a::Ptr Float) -> do
    allocaArray n $ \(b::Ptr Float) -> do
    allocaArray n $ \(temp::Ptr Float) -> do
    runQueueForType DeviceTypeGPU $ do
    p <- buildProgramFromSource "" [file]
    c <- newBuffer MemReadWrite NoHostPtr n
    d <- newBuffer MemReadWrite NoHostPtr n
    let printAll = do
        liftIO $ putStrLn "-----------"
        liftIO $ putStr "a: " >> peekArray n a >>= print
        liftIO $ putStr "b: " >> peekArray n b >>= print
        waitForCommand $ temp =: c
        liftIO $ putStr "c: " >> peekArray n temp >>= print
        waitForCommand $ temp =: d
        liftIO $ putStr "d: " >> peekArray n temp >>= print
    printAll
    mapM_ waitForCommand [c =: a, d =: c, b =: d]
    printAll
    mapM_ waitForCommand [slice 5 2 c =: a, slice 8 3 c =: slice 4 3 d, 
                            b =: slice 11 1 c]
    printAll
    liftIO $ print [slice 4 16 c, slice (-3) 16 c, slice 10 10 c]
    waitForCommand $ slice 5 0 c =: slice 4 0 d
    printAll


