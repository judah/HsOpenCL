{-# LANGUAGE ScopedTypeVariables #-}
module Test1 where

import OpenCL.Platform
import OpenCL.Context

import Control.Exception
showDevice dev = clDeviceVendor dev >>= print 
                >> clDeviceName dev >>= print

test1 = do
    getDeviceID DeviceTypeCPU >>= showDevice
    getDeviceID DeviceTypeGPU >>= showDevice

test2 file = do
    contents <- readFile file
    dev <- getDeviceID DeviceTypeGPU
    context <- clCreateContext dev
    prog <- createProgramWithSource context [contents]
    print "Created!"
    handle (\(e::SomeException) -> print ("exception:",e))
            $ buildProgram prog
    print "Done!"
    getBuildLog prog dev >>= print
