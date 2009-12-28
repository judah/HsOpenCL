{-# LANGUAGE ScopedTypeVariables #-}
import OpenCL

import qualified Data.ByteString as B
import Control.Exception
import Control.Monad

main = do
    putStrLn "---------\n------ bad build ------\n------\n"
    testBuild ["test_prog.cl","test_prog_bad.cl","test_prog2.cl"]
    putStrLn "---------\n------ good build ------\n------\n"
    testBuild ["test_prog.cl","test_prog2.cl"]
    putStrLn "---------\n------ binary build ------\n------\n"
    testBinaryBuild ["test_prog.cl"]

testBuild files = do
    cxt <- createContextFromType DeviceTypeAll
    bs <- mapM B.readFile files
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

testBinaryBuild files = do
    cxt <- createContextFromType DeviceTypeGPU
    let devs = contextDevices cxt
    bs <- mapM B.readFile files
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

