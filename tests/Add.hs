{-# LANGUAGE TemplateHaskell,QuasiQuotes, Rank2Types #-}
--
-- To run, use e.g. "runghc Add.hs 1024"

import System.HsOpenCL
import System.HsOpenCL.Instances.CArray
import System.Environment
import Foreign

import Data.Array.CArray (listArray, elems)
import Data.Array.IOCArray (newListArray)
import System.IO
import Data.List (findIndices)

-- $(declareKernelsFromFile "Adder" "test_prog.cl")
$(declareKernels "Adder" [$clProg| __kernel void
            add(__global float *a,__global float *b, __global float *answer)
            {
	        int gid = get_global_id(0);
	        answer[gid] = a[gid] + b[gid];
            }|])


main = do
    hSetBuffering stdout LineBuffering
    [ns] <- getArgs
    runQueueForType DeviceTypeGPU $ do
    let size = read ns
    let n = toEnum size
    -- Allocate host memory; this can be either pure or impure.
    let bounds = (0,size-1)
    liftIO $ putStrLn $ "Array bounds: " ++ show bounds
    let a = asCArray $ listArray bounds [0..n-1]
    b <- asIOCArray $ newListArray bounds [n-1,n-2..0]
    -- Allocate device memory:
    -- Either allocaBuffer or newBuffer will work for this example.
    allocaBuffer MemReadOnly NoHostPtr size $ \aMem -> do
    bMem <- newBuffer MemReadOnly NoHostPtr size
    allocaBuffer MemReadWrite NoHostPtr size $ \ansMem -> do
    -- Run the program:
    prog <- buildAdder ""
    liftIO $ putStrLn "Running..."
    waitForCommands [aMem =: a, bMem =: b
                    , add prog size Nothing aMem bMem ansMem
                    ]
    results <- copyToCArray bounds ansMem
    liftIO $ putStrLn "Done."
    liftIO $ do
        let es = elems results
        putStrLn "First 10 results are:"
        print $ take 10 es
        let ok = (== (n-1))
        if all ok es
            then putStrLn "Test passed!"
            else do
                putStrLn "Test failed!"
                putStrLn $ "Bad indices:" ++ show (findIndices (not . ok) es)
