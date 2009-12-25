{-# LANGUAGE TemplateHaskell #-}
module DeviceProperties where

import OpenCL

import Language.Haskell.TH

allNames :: [Name]
allNames = [
            'clDeviceType
            , 'clDeviceVendorId
            ,'clDeviceMaxComputeUnits
            ,'clDeviceMaxWorkItemDimensions
            ,'clDeviceMaxWorkGroupSize
            ,'clDeviceMaxWorkItemSizes
            ,'clDevicePreferredVectorWidthChar
            ,'clDevicePreferredVectorWidthShort
            ,'clDevicePreferredVectorWidthInt
            ,'clDevicePreferredVectorWidthLong
            ,'clDevicePreferredVectorWidthFloat
            ,'clDevicePreferredVectorWidthDouble
            ,'clDeviceMaxClockFrequency
            ,'clDeviceAddressBits
            ,'clDeviceMaxReadImageArgs
            ,'clDeviceMaxWriteImageArgs
            ,'clDeviceMaxMemAllocSize
            ,'clDeviceImage2dMaxWidth
            ,'clDeviceImage2dMaxHeight
            ,'clDeviceImage3dMaxWidth
            ,'clDeviceImage3dMaxHeight
            ,'clDeviceImage3dMaxDepth
            ,'clDeviceImageSupport
            ,'clDeviceMaxParameterSize
            ,'clDeviceMaxSamplers
            ,'clDeviceMemBaseAddrAlign
            ,'clDeviceMinDataTypeAlignSize
            -- ,'clDeviceSingleFpConfig
            -- ,'clDeviceGlobalMemCacheType
            ,'clDeviceGlobalMemCachelineSize
            ,'clDeviceGlobalMemCacheSize
            ,'clDeviceGlobalMemSize
            ,'clDeviceMaxConstantBufferSize
            ,'clDeviceMaxConstantArgs
            -- ,'clDeviceLocalMemType
            ,'clDeviceLocalMemSize
            ,'clDeviceErrorCorrectionSupport
            ,'clDeviceProfilingTimerResolution
            ,'clDeviceEndianLittle
            ,'clDeviceAvailable
            ,'clDeviceCompilerAvailable
            -- ,'clDeviceExecutionCapabilities
            -- ,'clDeviceQueueProperties
            ,'clDeviceName
            ,'clDeviceVendor
            ,'clDriverVersion
            ,'clDeviceProfile
            ,'clDeviceVersion
            ,'clDeviceExtensions
            -- ,'clDevicePlatform
            ]

genPropList :: ExpQ
genPropList = listE $ map mkNamePair allNames
  where
    mkNamePair n = tupE [stringE (nameBase n), appE [|(show .)|] (varE n)]

