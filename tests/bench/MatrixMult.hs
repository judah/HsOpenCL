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

declareKernels "MatrixMult" [$clProg|
    __kernel void multNaive(__global float *a, __global float *b,
                            __global float *c) {
    #define IX(q,r,s) q[(r)*SIZE+(s)]
        int i = get_global_id(0);
        int j = get_global_id(1);

        float sum=0;
        for (int k=0; k<SIZE; k++) {
            sum += IX(a,i,k) * IX(b,k,j);
        }
        IX(c,i,j)=sum;
    }|]


main = do
    [dtypeS,sizeS] <- getArgs
    let sizeM = read sizeS
    hSetBuffering stdout LineBuffering
    runQueueForType (read dtypeS) $ do
    setProperties [QueueProfilingEnable] True

    let bounds = ((0,0),(sizeM-1,sizeM-1))
    let matSize = rangeSize bounds
    m <- asIOCArray $ randomA bounds
    liftIO $ putStrLn "Copying..."
    withBuffer MemReadOnly NoHostPtr matSize $ \a -> do
    withBuffer MemReadOnly NoHostPtr matSize $ \b -> do
    withBuffer MemWriteOnly NoHostPtr matSize $ \c -> do
    waitForCommands [a =: m, b =: m]

    let options = "-DSIZE=" ++ show sizeM
    p <- buildMatrixMult options

    liftIO $ putStrLn "Running..."
    profCommand 20 $ multNaive p (sizeM,sizeM) Nothing a b c
    results <- asIOCArray $ newArray_ bounds
    waitForCommand $ results =: c
    liftIO $ mapM_ touchIOCArray [m]
