{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import OpenCL.Simple

import Data.Array.CArray
import Data.Array.IOCArray

import qualified Data.ByteString as B


main = do
    contents <- B.readFile "test_prog.cl"
    prog <- newSimpleProgram DeviceTypeGPU [contents]
    putStrLn "Built!"
    let size = 32
    add :: CArray Int Float -> CArray Int Float -> IOCArray Int Float -> IO ()
        <- getKernelFunc prog "add"
    let n = toEnum size
    let a = listArray (0,size-1) [0..n-1]
    let b = listArray (0,size-1) [n,n-1..1]
    c <- newArray_ (0,size-1)
    add a b c :: IO ()
    getElems c >>= print
