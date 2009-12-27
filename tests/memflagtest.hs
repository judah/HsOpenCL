{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import OpenCL
import Foreign
import Foreign.C

main = do
    cxt <- createContextFromType DeviceTypeCPU
    p :: Ptr Float <- newArray [1..10]
    print p
    buf :: CLMem Float <- createBuffer cxt CLMemReadWrite (CopyAllocHostPtr p) 5
    clGetMemReferenceCount buf >>= print
    clRetainMemObject buf
    clGetMemReferenceCount buf >>= print
    print $ clMemFlags buf
    print $ clMemSize buf

-- Correct: size, access type
-- Incorrect: hostptr, referenceCount.
