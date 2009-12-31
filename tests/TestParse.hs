{-# LANGUAGE TemplateHaskell #-}
module Main where

import Parse
import OpenCL
import OpenCL.Simple
import System.Environment
import Foreign

declareKernelsFromFile "prog" "test_prog.cl"

main = do
    [ns] <- getArgs
    let size = read ns :: Int
    let n = toEnum size
    let context = simpleCxt prog
    let queue = simpleQueue prog
    withArray [0..n-1] $ \a -> do
    withArray [n-1,n-2..0] $ \b -> do
    allocaArray size $ \results -> do
    -- Allocate the device memory, and copy the data manually:
    withBuffer context MemReadOnly NoHostPtr size $ \aMem -> do
    withBuffer context MemReadOnly NoHostPtr size $ \bMem -> do
    withBuffer context MemReadWrite NoHostPtr size $ \ansMem -> do
    waitForCommands queue [aMem =: a, bMem =: b]
    waitForCommand (simpleQueue prog) (add size Nothing aMem bMem ansMem)
    waitForCommand queue (results =: ansMem)
    putStrLn "First 10 results are:"
    peekArray size results >>= print . take 10
    
    
    
