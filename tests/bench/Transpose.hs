module Main where

-- TODO: use the criterion library.
-- TODO: bench without returning Event and see how much faster it is.
-- TODO: crashes when compiled!
import System.Time
import Criterion.Main hiding (run)

import OpenCL
import OpenCL.Simple hiding (KernelArg)
import qualified Data.ByteString as B
import System.Random
import Control.Monad
import Foreign

sizeX = 256
sizeY = 4096
blockDim = 16
size2 = sizeX*sizeY


main = do
    source <- B.readFile "transpose.cl"
    p <- newSimpleProgram DeviceTypeGPU [source]
    ks <- createKernelsInProgram (simpleProgram p)
    -- allocate host memory:
    allocaArray (sizeX*sizeY) $ \h_idata -> do
    -- fill with random data:
    forM_ [0..sizeX*sizeY-1] $ \i -> randomIO >>= pokeElemOff h_idata i
    -- allocate device memeory and copy host to it:
    d_idata <- createBuffer (simpleCxt p) MemReadOnly
                    (CopyHostPtr h_idata) size2
    d_odata <- createBuffer (simpleCxt p) MemWriteOnly NoHostPtr size2
    -- Run the tests
    bs <- mapM (testKernel p d_idata d_odata) ks
    defaultMain bs
    putStrLn "Finished."

testKernel :: SimpleProgram -> Buffer Float -> Buffer Float -> Kernel
            -> IO Benchmark
testKernel p idata odata k = do
    putStrLn $ "----- Testing " ++ kernelFunctionName k ++ " -----"
    setKernelArgs k args
    -- they do some sort of rounding up for the global size,
    -- but I think with the above dimensions we're OK.
    
    let globalSize = [sizeX,sizeY]
    let localSize = Just [blockDim,blockDim]
    let run = enqueueNDRangeKernel (simpleQueue p) k globalSize localSize []
    
    return $ bench ("kernel-" ++ kernelFunctionName k)
                    $ run >> return ()
  where
    localBlock :: Local Float
    localBlock = Local $ blockDim * (blockDim+1)
    args = idata :& odata :& Scalar sizeX :& Scalar sizeY
                    :& if kernelNumArgs k == 5 
                        then localBlock :& KNil
                        else KNil
