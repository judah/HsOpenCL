{-# LANGUAGE TemplateHaskell, QuasiQuotes, Rank2Types #-}
module Main where

-- Example of a speed-up from shared memory.
-- Taken from the NVIDIA OpenCL Best Practices Guide.
-- To run:
-- ghc --make -O2 MatrixMult.hs
-- ./MatrixMult 1024


import System.HsOpenCL
import System.HsOpenCL.Instances.CArray
import Data.Array.CArray as C
import Data.Array.IOCArray
import Control.Monad

import ProfCommand

import System.Environment
import System.IO
import Data.List
import Text.Printf
import Control.Exception

declareKernels "MatrixMult" [$clProg|
    __kernel void multNaive(__global float* a, __global float* b,
                        __global float* c,
                        int N) {
        int row = get_global_id(1); 
        int col = get_global_id(0); 
        float sum = 0.0f; 
        for (int i = 0; i < TILE_DIM; i++) {
            sum += a[row*TILE_DIM+i] * b[i*N+col]; 
        }
        c[row*N+col] = sum;
    }

    __kernel void coalescedMultiply(__global float* a, __global float* b,
                                    __global float* c,int N,
                                    __local float *aTile) {
        int row = get_global_id(1);
        int col = get_global_id(0);
        float sum = 0.0f;
        int x = get_local_id(0);
        int y = get_local_id(1);
        aTile[y*TILE_DIM+x] = a[row*TILE_DIM+x];
        for (int i = 0; i < TILE_DIM; i++) {
            sum += aTile[y*TILE_DIM+i]* b[i*N+col];
        }
        c[row*N+col] = sum;
    }
|]


main = do
    [sizeS] <- getArgs
    let dtype = DeviceTypeGPU
    let sizeM = read sizeS
    let tileSize = 8
    hSetBuffering stdout LineBuffering
    runQueueForType dtype $ do
    setProperties [QueueProfilingEnable] True

    let mkBounds m n = ((0,0),(m-1,n-1))
    a <- randomCArray $ mkBounds sizeM tileSize
    b <- randomCArray $ mkBounds tileSize sizeM

    liftIO $ putStrLn "Copying..."
    allocaBuffer MemReadOnly NoHostPtr (sizeM * tileSize) $ \aM -> do
    allocaBuffer MemReadOnly NoHostPtr (tileSize * sizeM) $ \bM -> do
    allocaBuffer MemWriteOnly NoHostPtr (sizeM*sizeM) $ \cM -> do
    waitForCommands [aM =: a, bM =: b]
    putStrLn' "Copy done." 
    let options = printf "-DTILE_DIM=%d" tileSize
    p <- buildMatrixMult options
    putStrLn' "Build done."

    liftIO $ evaluate a
    liftIO $ evaluate b
    (c,t) <- liftIO $ timed $ evaluate $ multMatrices a b
    putStrLn' $ printf "Time to compute with carrays: %.5f\n" t

    putStrLn' "Running..."
    let test cmd = do
        waitForCommand cmd
        cX <- copyToCArray (mkBounds sizeM sizeM) cM
        profCommand 20 cmd
        print' ("Are they equal:",c==cX)

    let globalSize = (sizeM,sizeM)

    putStrLn' "--- Naive version ---"
    test $ multNaive p globalSize Nothing aM bM cM sizeM
    
    putStrLn' "--- Other version ---"
    let localSize = Just (tileSize,tileSize)
    test $ coalescedMultiply p globalSize localSize aM bM cM sizeM
                        (Local (tileSize*tileSize))

multMatrices :: CArray (Int,Int) Float -> CArray (Int,Int) Float
                    -> CArray (Int,Int) Float
multMatrices a b
    | (c0,c1) /=(c0',c1') = error $ "Bounds mismatched"
    | otherwise = array newBounds [((i,j),elt i j) | i <- range m, j <- range n]
  where
    ((m0,c0),(m1,c1)) = bounds a
    ((c0',n0),(c1',n1)) = bounds b
    m = (m0,m1)
    n = (n0,n1)
    c = (c0,c1)
    newBounds = ((m0,n0),(m1,n1))
    elt i j = sum [a!(i,k)*b!(k,j) | k <- range c]

sum' = foldl' (+) 0

