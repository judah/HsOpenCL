{-# LANGUAGE TemplateHaskell, QuasiQuotes, Rank2Types #-}
module Main where

import OpenCL
import OpenCL.Instances.CArray
import Data.Array.CArray as C
import Data.Array.IOCArray
import Control.Monad

import ProfCommand

import System.Environment
import System.IO
import Data.List

declareKernels "MatrixMult" [$clProg|
    __kernel void multNaive(__global float *a, __global float *b,
                            __global float *c) {
        int i = get_global_id(1);
        int j = get_global_id(0);

        float sum=0;
        for (int k=0; k<TILE_DIM; k++) {
            sum += a[i*TILE_DIM+k]*b[k*SIZE+j];
        }
        c[i*SIZE+j]=sum;
    }

    __kernel void nvidia1(__global float* a, __global float* b,
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
                   // __local float aTile[TILE_DIM][TILE_DIM]) {
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
    let options = unwords [defineArg "SIZE" sizeM
                          , defineArg "TILE_DIM" tileSize
                          ]
    p <- buildMatrixMult options
    putStrLn' "Build done."

    let c = multMatrices a b
    -- try to force c; TODO time this
    print' $ bounds c

    putStrLn' "Running..."
    let test cmd = do
        waitForCommand cmd
        cX <- copyToCArray (mkBounds sizeM sizeM) cM
        -- print' cX
        profCommand 20 cmd
        print' ("Are they equal:",c==cX)

    let globalSize = (sizeM,sizeM)

    putStrLn' "--- Naive version ---"
    test $ multNaive p globalSize Nothing aM bM cM
    
    putStrLn' "--- NVidia version ---"
    test $ nvidia1 p globalSize Nothing aM bM cM sizeM
    
    putStrLn' "--- Other version ---"
    let localSize = Just (tileSize,tileSize)
    test $ coalescedMultiply p globalSize localSize aM bM cM sizeM
                        (Local (tileSize*tileSize))

putStrLn' = liftIO . putStrLn

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

defineArg :: Show a => String -> a -> String
defineArg name val = "-D" ++ name ++ "=" ++ show val

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

