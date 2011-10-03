{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}
import System.HsOpenCL
import qualified Data.ByteString as B
import System.IO

myprog = [$clProg|
    __kernel void add(__global float *a,
                        __global float *b,
                        __global float *answer)
    {
        int gid = get_global_id(0);
        answer[gid] = a[gid] + b[gid];
    }
    __kernel void mult(__global float *a,
                        __global float *b,
                        __global float *answer)
    {
        int gid = get_global_id(0);
        answer[gid] = a[gid] * b[gid];
    }
|]

main = do
    dev <- getDeviceID [DeviceTypeGPU]
    context <- createContext [dev]
    prog <- createProgramWithSource context [myprog]
    buildProgram prog ""
    putStrLn "Built program"
    k <- createKernel prog "mult"
    putStrLn "one"
    ks <- createKernelsInProgram prog
    putStrLn "two"

    mapM_ printKernelInfo (k:ks)

printKernelInfo k = do
    putStrLn $ "--- " ++ kernelFunctionName k ++ " ---"
    let [dev] = contextDevices $ kernelContext k
    print (kernelNumArgs k, dev)
    B.putStrLn $ programSource $ kernelProgram k
    hFlush stdout
    putStrLn $ "work group size: "
        ++ show (kernelWorkGroupSize k dev)
    putStrLn $ "compile work group size: "
        ++ show (kernelCompileWorkGroupSize k dev)
    putStrLn "Local mem size:"
    getKernelLocalMemSize k dev >>= print
