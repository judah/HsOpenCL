{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.HsOpenCL

import Language.Haskell.TH


main = getDeviceIDs DeviceTypeAll Nothing >>= mapM_ printAttrs
  where
    printAttr d (n,f) = putStrLn $ n ++ ": " ++ f d
    printAttrs d = do
        putStrLn $ "------" ++ deviceName d ++ "-----"
        mapM_ (printAttr d) propList
        putStrLn ""


-- Making one large splice so we can fit this in one file
-- without falling afoul of GHC's stage restrictions.
propList = $(let
        mkNamePair n = tupE [stringE (nameBase n), appE [|(show .)|] (varE n)]
        allNames = [
            'deviceType
            , 'deviceVendorId
            ,'deviceMaxComputeUnits
            ,'deviceMaxWorkItemDimensions
            ,'deviceMaxWorkGroupSize
            ,'deviceMaxWorkItemSizes
            ,'devicePreferredVectorWidthChar
            ,'devicePreferredVectorWidthShort
            ,'devicePreferredVectorWidthInt
            ,'devicePreferredVectorWidthLong
            ,'devicePreferredVectorWidthFloat
            ,'devicePreferredVectorWidthDouble
            ,'deviceMaxClockFrequency
            ,'deviceAddressBits
            ,'deviceMaxReadImageArgs
            ,'deviceMaxWriteImageArgs
            ,'deviceMaxMemAllocSize
            ,'deviceImage2dMaxWidth
            ,'deviceImage2dMaxHeight
            ,'deviceImage3dMaxWidth
            ,'deviceImage3dMaxHeight
            ,'deviceImage3dMaxDepth
            ,'deviceImageSupport
            ,'deviceMaxParameterSize
            ,'deviceMaxSamplers
            ,'deviceMemBaseAddrAlign
            ,'deviceMinDataTypeAlignSize
            ,'deviceSingleFpConfig
            ,'deviceGlobalMemCacheType
            ,'deviceGlobalMemCachelineSize
            ,'deviceGlobalMemCacheSize
            ,'deviceGlobalMemSize
            ,'deviceMaxConstantBufferSize
            ,'deviceMaxConstantArgs
            ,'deviceLocalMemType
            ,'deviceLocalMemSize
            ,'deviceErrorCorrectionSupport
            ,'deviceProfilingTimerResolution
            ,'deviceEndianLittle
            ,'deviceAvailable
            ,'deviceCompilerAvailable
            ,'deviceExecutionCapabilities
            ,'deviceQueueProperties
            ,'deviceName
            ,'deviceVendor
            ,'driverVersion
            ,'deviceProfile
            ,'deviceVersion
            ,'deviceExtensions
            ,'devicePlatform
            ]

        in listE $ map mkNamePair allNames
        )


