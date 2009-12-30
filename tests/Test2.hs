{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}
module Main where

import OpenCL.Simple
import MultiLine

import Data.Array.CArray
import Data.Array.IOCArray

import qualified Data.ByteString as B

add = [$clProg|
    __kernel void add(__global float *a,
	            __global float *b,
	            __global float *answer)
    {
	int gid = get_global_id(0);
	answer[gid] = a[gid] + b[gid];
    }|]

main = do
    prog <- newSimpleProgram DeviceTypeGPU [add]
    putStrLn "Built!"
    let size = 32
    let n = toEnum size
    let a = listArray (0,size-1) [0..n-1]
    let b = listArray (0,size-1) [n,n-1..1]
    -- Immutable return value:
    add :: CArray Int Float -> CArray Int Float -> IO (CArray Int Float)
        <- getKernelFunc prog "add"
    add a b >>= print . elems
    -- Mutable argument:
    c <- newArray_ (0,size-1)
    add2 :: CArray Int Float -> CArray Int Float -> IOCArray Int Float -> IO ()
        <- getKernelFunc prog "add"
    add2 a b c
    getElems c >>= print
