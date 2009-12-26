{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import OpenCL.Simple

import Data.Array.CArray
import Data.Array.IOCArray


main = do
    cxt <- newSimpleContext DeviceTypeGPU
    contents <- readFile "test_prog.cl"
    prog <- buildSimpleProgram cxt [contents]
    putStrLn "Built!"
    kernel <- getKernel prog "add"
    let size = 32
    let n = toEnum size :: Float
    let a = listArray (0,size-1) [0..n-1]
    let b = listArray (0,size-1) [n,n-1..1]
    c :: IOCArray Int Float <- newArray_ (0,size-1)
    runKernel cxt kernel [Immutable a, Immutable b, Mutable c]
    getElems c >>= print
