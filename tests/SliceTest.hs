{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import OpenCL
import OpenCL.Simple

import Foreign
import qualified Data.ByteString.Char8 as B

n = 16

main = do
    file <- B.readFile "test_prog.cl"
    p <- newSimpleProgram DeviceTypeGPU [file]
    let run = waitForCommand (simpleQueue p)
    withArray [0..toEnum n-1] $ \(a::Ptr Float) -> do
    allocaArray n $ \(b::Ptr Float) -> do
    allocaArray n $ \(temp::Ptr Float) -> do
    withBuffer (simpleCxt p) MemReadWrite NoHostPtr n $ \c -> do
    withBuffer (simpleCxt p) MemReadWrite NoHostPtr n $ \d -> do
    let printAll = do
        putStrLn "-----------"
        putStr "a: " >> peekArray n a >>= print
        putStr "b: " >> peekArray n b >>= print
        run $ temp =: c
        putStr "c: " >> peekArray n temp >>= print
        run $ temp =: d
        putStr "d: " >> peekArray n temp >>= print
    printAll
    run $ c =: a
    run $ d =: c
    run $ b =: d
    printAll
    run $ slice 5 2 c =: a
    run $ slice 8 3 c =: slice 4 3 d
    run $ b =: slice 11 1 c
    printAll
    print [slice 4 16 c, slice (-3) 16 c, slice 10 10 c]
    run $ slice 5 0 c =: slice 4 0 d
    printAll


