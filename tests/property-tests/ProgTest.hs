{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}
import System.HsOpenCL

import qualified Data.ByteString as B
import Control.Exception
import Control.Monad

test1 = [$clProg| __kernel void add(__global float *a,
                                        __global float *b,
                                        __global float *answer)
                        {
                            int gid = get_global_id(0);
                            answer[gid] = a[gid] + b[gid];
                        }
            |]

test2 = [$clProg| __kernel void mult2(__global float *x,
                                        __global float *answer)
                    {
                        int gid = get_global_id(0);
                        answer[gid] = 2*x[gid];
                    }
            |]

test3 = [$clProg| junk akjdhfkasdhkjfh |]

main = do
    putStrLn "---------\n------ bad build ------\n------\n"
    testBuild [test1,test2,test3]
    putStrLn "---------\n------ good build ------\n------\n"
    testBuild [test1,test2]
    putStrLn "---------\n------ binary build ------\n------\n"
    testBinaryBuild [test1]

testBuild bs = do
    cxt <- createContextFromType DeviceTypeAll
    prog <- createProgramWithSource cxt bs
    handle (handler prog) $ do
        buildProgram prog "-D foo"
        putStrLn "Build succeeded."
    showInfos prog
  where
    handler :: Program -> SomeException -> IO ()
    handler prog e = do
        putStrLn "Got error:"
        print e

testBinaryBuild bs = do
    cxt <- createContextFromType DeviceTypeGPU
    let devs = contextDevices cxt
    prog <- createProgramWithSource cxt bs
    buildProgram prog ""
    bins <- getProgramBinaries prog
    putStrLn "Binaries:"
    print (length bins, map B.length bins)
    putStrLn "About to create:"
    prog2 <- createProgramWithBinary cxt (zip devs bins)
    print $ programDevices prog2
    putStrLn "Binaries:"
    getProgramBinaries prog2 >>= mapM_ print
    putStrLn "About to build:"
    handle (\(e::CLError) -> do
            putStrLn "*********\n**********"
            print ("Error building:",e)
            showBuildInfos prog2 devs
            )
            $ buildProgram prog2 ""
    showInfos prog2

-- TODO: try with multiple (DeviceTypeAny)
showInfos prog = do
    putStrLn "Source:"
    print (programSource prog)
    putStrLn "Binaries"
    getProgramBinaries prog >>= mapM_ print
    let devs = programDevices prog
    let devs2 = contextDevices $ programContext prog
    putStrLn "Devices:"
    print devs
    print devs2
    putStrLn "Build infos:"
    showBuildInfos prog devs

showBuildInfos prog = mapM_ (showBuildInfoDev prog)

showBuildInfoDev prog dev = do
        print dev
        getBuildStatus prog dev >>= print
        getBuildOptions prog dev >>= print
        putStrLn "*** Log: ***"
        getBuildLog prog dev >>= B.putStrLn
        putStrLn "*** End Log. ***"
        putStrLn "-------------\n"

