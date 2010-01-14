{-# LANGUAGE Rank2Types #-}
module Main where

-- Taken from the NVIDIA OpenCL example code.
-- To run:
-- ghc --make -O2 Transpose.hs
-- ./Transpose

import System.HsOpenCL
import System.HsOpenCL.Instances.CArray
import ProfCommand
import Control.Monad
import Text.Printf
import Data.Array.CArray

sizeX = 256
sizeY = 4096
blockDim = 16
size2 = sizeX*sizeY

declareKernelsFromFile "Transpose" "transpose.cl"


main = runQueueForType DeviceTypeGPU $ do
    setProperties [QueueProfilingEnable] True
    p <- buildTranspose ""

    let mkBounds m n = ((0,0),(m-1,n-1))
    a <- randomCArray $ mkBounds sizeX sizeY
    aM <- newBuffer MemReadOnly NoHostPtr (sizeX*sizeY)
    bM <- newBuffer MemWriteOnly NoHostPtr (sizeX*sizeY)
    waitForCommand $ aM =: a

    let globalSize = (sizeX,sizeY)
    let localSize = Just (blockDim,blockDim)
    let cmds = [("transpose_naive", transpose_naive p globalSize localSize
                                            aM bM sizeX sizeY)
               , ("transpose", transpose p globalSize localSize
                                        aM bM sizeX sizeY (Local $ blockDim*(blockDim+1)))
               ]
    forM_ cmds $ \(n, k) -> do
    putStrLn' $ printf "Running %s..." n
    profCommand 20 k
