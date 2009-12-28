{-# LANGUAGE TemplateHaskell #-}
module DeviceProperties where

import OpenCL

import Language.Haskell.TH

allNames :: [Name]
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
            -- ,'deviceGlobalMemCacheType
            ,'deviceGlobalMemCachelineSize
            ,'deviceGlobalMemCacheSize
            ,'deviceGlobalMemSize
            ,'deviceMaxConstantBufferSize
            ,'deviceMaxConstantArgs
            -- ,'deviceLocalMemType
            ,'deviceLocalMemSize
            ,'deviceErrorCorrectionSupport
            ,'deviceProfilingTimerResolution
            ,'deviceEndianLittle
            ,'deviceAvailable
            ,'deviceCompilerAvailable
            -- ,'deviceExecutionCapabilities
            ,'deviceQueueProperties
            ,'deviceName
            ,'deviceVendor
            ,'driverVersion
            ,'deviceProfile
            ,'deviceVersion
            ,'deviceExtensions
            -- ,'devicePlatform
            ]

genPropList :: ExpQ
genPropList = listE $ map mkNamePair allNames
  where
    mkNamePair n = tupE [stringE (nameBase n), appE [|(show .)|] (varE n)]

