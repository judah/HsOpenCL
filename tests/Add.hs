{-# LANGUAGE TemplateHaskell,QuasiQuotes, Rank2Types #-}
--
-- To run, use e.g. "runghc Add.hs 1024"

import System.HsOpenCL
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import System.Environment
import Foreign

import System.IO
import Data.List (findIndices)
import Control.Monad (zipWithM_)

declareKernels "Adder" [clProg| __kernel void
            add(__global float *a,__global float *b, __global float *answer)
            {
	        int gid = get_global_id(0);
	        answer[gid] = a[gid] + b[gid];
            }|]

main = do
    hSetBuffering stdout LineBuffering
    [ns] <- getArgs
    runQueueForType DeviceTypeGPU $ do
    let size = read ns
    let n = toEnum size
    -- Allocate host memory; this can be either pure or impure.
    let bounds = (0,size-1)
    liftIO $ putStrLn $ "Array bounds: " ++ show bounds
    -- Fill the vectors with some dummy values
    let a = V.fromList [0..n-1] -- pure vector
    b <- liftIO $ M.new size -- mutable vector
    liftIO $ zipWithM_ (M.write b) [0..size-1] [n-1,n-2..0]
    -- Allocate device memory:
    allocaBuffer MemReadOnly NoHostPtr size $ \aMem -> do
    allocaBuffer MemReadOnly  NoHostPtr size $ \bMem -> do
    allocaBuffer MemReadWrite NoHostPtr size $ \ansMem -> do
    -- Run the program:
    prog <- buildAdder ""
    let x = (aMem :: Buffer Float) =: a
    return ()
    waitForCommands [aMem =: a]
    waitForCommands [bMem =: b]
    waitForCommands [add prog size Nothing aMem bMem ansMem
                    ]
    results <- copyToVector ansMem
    liftIO $ putStrLn "Done."
    liftIO $ do
        let es = V.toList results
        putStrLn "First 10 results are:"
        print $ take 10 es
        let ok = (== (n-1))
        if all ok es
            then putStrLn "Test passed!"
            else do
                putStrLn "Test failed!"
                putStrLn $ "Bad indices:" ++ show (findIndices (not . ok) es)
