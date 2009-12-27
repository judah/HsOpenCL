{-# LANGUAGE TemplateHaskell #-}
module Main where

import OpenCL
import DeviceProperties
propList = $(genPropList)

main = getDeviceIDs DeviceTypeAll >>= mapM_ printAttrs
  where
    printAttr d (n,f) = putStrLn $ n ++ ": " ++ f d
    printAttrs d = do
        putStrLn $ "------" ++ deviceName d ++ "-----"
        mapM_ (printAttr d) propList
        putStrLn ""

