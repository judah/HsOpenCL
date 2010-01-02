module OpenCL.Platform(
            -- * Platforms
            PlatformID,
            getPlatformIDs,
            platformProfile,
            platformVersion,
            platformName,
            platformVendor,
            platformExtensions,
            -- * Devices
            DeviceType(..),
            DeviceID,
            getDeviceID,
            getDeviceIDs,
            -- * Device properties
            Size,
            ULong, 
            deviceType,
            deviceVendorId,
            deviceMaxComputeUnits,
            deviceMaxWorkItemDimensions,
            deviceMaxWorkGroupSize,
            deviceMaxWorkItemSizes,
            devicePreferredVectorWidthChar,
            devicePreferredVectorWidthShort,
            devicePreferredVectorWidthInt,
            devicePreferredVectorWidthLong,
            devicePreferredVectorWidthFloat,
            devicePreferredVectorWidthDouble,
            deviceMaxClockFrequency,
            deviceAddressBits,
            deviceMaxReadImageArgs,
            deviceMaxWriteImageArgs,
            deviceMaxMemAllocSize,
            deviceImage2dMaxWidth,
            deviceImage2dMaxHeight,
            deviceImage3dMaxWidth,
            deviceImage3dMaxHeight,
            deviceImage3dMaxDepth,
            deviceImageSupport,
            deviceMaxParameterSize,
            deviceMaxSamplers,
            deviceMemBaseAddrAlign,
            deviceMinDataTypeAlignSize,
            deviceSingleFpConfig,
            DeviceFPConfig(..),
            -- deviceGlobalMemCacheType,
            deviceGlobalMemCachelineSize,
            deviceGlobalMemCacheSize,
            deviceGlobalMemSize,
            deviceMaxConstantBufferSize,
            deviceMaxConstantArgs,
            -- deviceLocalMemType,
            deviceLocalMemSize,
            deviceErrorCorrectionSupport,
            deviceProfilingTimerResolution,
            deviceEndianLittle,
            deviceAvailable,
            deviceCompilerAvailable,
            -- deviceExecutionCapabilities,
            deviceQueueProperties,
            deviceName,
            deviceVendor,
            driverVersion,
            deviceProfile,
            deviceVersion,
            deviceExtensions,
            devicePlatform,
            ) where

#include <OpenCL/OpenCL.h>

import Control.Applicative
import Data.Bits

import OpenCL.Internal.C2HS
import OpenCL.Error
import OpenCL.Internal.Types
import OpenCL.Platform.Foreign

getPlatformIDs :: IO [PlatformID]
getPlatformIDs = map PlatformID <$> getObjArray clGetPlatformIDs

platformProfile :: PlatformID -> String
platformProfile p = getPureProp $ getPlatformInfo p CLPlatformProfile

platformVersion :: PlatformID -> String
platformVersion p = getPureProp $ getPlatformInfo p CLPlatformVersion

platformName :: PlatformID -> String
platformName p = getPureProp $ getPlatformInfo p CLPlatformName

platformVendor :: PlatformID -> String
platformVendor p = getPureProp $ getPlatformInfo p CLPlatformVendor

platformExtensions :: PlatformID -> String
platformExtensions p = getPureProp $ getPlatformInfo p CLPlatformExtensions

instance Show PlatformID where
    show p = "<" ++ platformName p ++ ">"



getDeviceID :: DeviceType -> IO DeviceID
getDeviceID dtype = alloca $ \p -> do
    clGetDeviceIDs nullPtr dtype 1 p
    DeviceID <$> peek (castPtr p)

getDeviceIDs :: DeviceType -> Maybe PlatformID -> IO [DeviceID]
getDeviceIDs dtype m_platform= do
    let platform = maybe nullPtr platformIDPtr m_platform
    -- First, query for the total number:
    let clGetDeviceIDs' plat d n p = clGetDeviceIDs plat d n (castPtr p)
    map (DeviceID . castPtr) <$> getObjArray (clGetDeviceIDs' platform dtype)

-- TODO: Orphan instance...
instance Show DeviceID where
    show dev = "<" ++ deviceVendor dev ++ " " ++ deviceName dev ++ ">"
---------------

deviceInfo :: Property a => Int -> DeviceID -> a
deviceInfo prop dev = getPureProp (clGetDeviceInfo dev prop)

type Size = #type size_t
type ULong = #type cl_ulong

-- Don't worrry about overflow; the spec says that CL_DEVICE_TYPE_ALL
-- won't be returned.
deviceType :: DeviceID -> [DeviceType]
deviceType d = unsafePerformIO $ getFlags
                        (clGetDeviceInfo d (#const CL_DEVICE_TYPE))
                        [DeviceTypeCPU, DeviceTypeGPU
                        , DeviceTypeAccelerator, DeviceTypeDefault
                        ]

deviceVendorId :: DeviceID -> Int
deviceVendorId = deviceInfo (#const CL_DEVICE_VENDOR_ID)

deviceMaxComputeUnits :: DeviceID -> Int
deviceMaxComputeUnits = deviceInfo (#const CL_DEVICE_MAX_COMPUTE_UNITS)

deviceMaxWorkItemDimensions :: DeviceID -> Int
deviceMaxWorkItemDimensions = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS)

deviceMaxWorkGroupSize :: DeviceID -> Size
deviceMaxWorkGroupSize = deviceInfo (#const CL_DEVICE_MAX_WORK_GROUP_SIZE)

deviceMaxWorkItemSizes :: DeviceID -> [Size]
deviceMaxWorkItemSizes = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_SIZES)

devicePreferredVectorWidthChar :: DeviceID -> Int
devicePreferredVectorWidthChar = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR)

devicePreferredVectorWidthShort :: DeviceID -> Int
devicePreferredVectorWidthShort = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT)

devicePreferredVectorWidthInt :: DeviceID -> Int
devicePreferredVectorWidthInt = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT)

devicePreferredVectorWidthLong :: DeviceID -> Int
devicePreferredVectorWidthLong = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG)

devicePreferredVectorWidthFloat :: DeviceID -> Int
devicePreferredVectorWidthFloat = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT)

devicePreferredVectorWidthDouble :: DeviceID -> Int
devicePreferredVectorWidthDouble = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE)

deviceMaxClockFrequency :: DeviceID -> Int
deviceMaxClockFrequency = deviceInfo (#const CL_DEVICE_MAX_CLOCK_FREQUENCY)

deviceAddressBits :: DeviceID -> Int
deviceAddressBits = deviceInfo (#const CL_DEVICE_ADDRESS_BITS)

deviceMaxReadImageArgs :: DeviceID -> Int
deviceMaxReadImageArgs = deviceInfo (#const CL_DEVICE_MAX_READ_IMAGE_ARGS)

deviceMaxWriteImageArgs :: DeviceID -> Int
deviceMaxWriteImageArgs = deviceInfo (#const CL_DEVICE_MAX_WRITE_IMAGE_ARGS)

deviceMaxMemAllocSize :: DeviceID -> ULong
deviceMaxMemAllocSize = deviceInfo (#const CL_DEVICE_MAX_MEM_ALLOC_SIZE)

deviceImage2dMaxWidth :: DeviceID -> Size
deviceImage2dMaxWidth = deviceInfo (#const CL_DEVICE_IMAGE2D_MAX_WIDTH)

deviceImage2dMaxHeight :: DeviceID -> Size
deviceImage2dMaxHeight = deviceInfo (#const CL_DEVICE_IMAGE2D_MAX_HEIGHT)

deviceImage3dMaxWidth :: DeviceID -> Size
deviceImage3dMaxWidth = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_WIDTH)

deviceImage3dMaxHeight :: DeviceID -> Size
deviceImage3dMaxHeight = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_HEIGHT)

deviceImage3dMaxDepth :: DeviceID -> Size
deviceImage3dMaxDepth = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_DEPTH)

deviceImageSupport :: DeviceID -> Bool
deviceImageSupport = deviceInfo (#const CL_DEVICE_IMAGE_SUPPORT)

deviceMaxParameterSize :: DeviceID -> Size
deviceMaxParameterSize = deviceInfo (#const CL_DEVICE_MAX_PARAMETER_SIZE)

deviceMaxSamplers :: DeviceID -> Int
deviceMaxSamplers = deviceInfo (#const CL_DEVICE_MAX_SAMPLERS)

deviceMemBaseAddrAlign :: DeviceID -> Int
deviceMemBaseAddrAlign = deviceInfo (#const CL_DEVICE_MEM_BASE_ADDR_ALIGN)

deviceMinDataTypeAlignSize :: DeviceID -> Int
deviceMinDataTypeAlignSize = deviceInfo (#const CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE)

deviceGlobalMemCachelineSize :: DeviceID -> Int
deviceGlobalMemCachelineSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE)

deviceGlobalMemCacheSize :: DeviceID -> ULong
deviceGlobalMemCacheSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_SIZE)

deviceGlobalMemSize :: DeviceID -> ULong
deviceGlobalMemSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_SIZE)

deviceMaxConstantBufferSize :: DeviceID -> ULong
deviceMaxConstantBufferSize = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE)

deviceMaxConstantArgs :: DeviceID -> Int
deviceMaxConstantArgs = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_ARGS)

deviceLocalMemSize :: DeviceID -> ULong
deviceLocalMemSize = deviceInfo (#const CL_DEVICE_LOCAL_MEM_SIZE)

deviceErrorCorrectionSupport :: DeviceID -> Bool
deviceErrorCorrectionSupport = deviceInfo (#const CL_DEVICE_ERROR_CORRECTION_SUPPORT)

deviceProfilingTimerResolution :: DeviceID -> Size
deviceProfilingTimerResolution = deviceInfo (#const CL_DEVICE_PROFILING_TIMER_RESOLUTION)

deviceEndianLittle :: DeviceID -> Bool
deviceEndianLittle = deviceInfo (#const CL_DEVICE_ENDIAN_LITTLE)

deviceAvailable :: DeviceID -> Bool
deviceAvailable = deviceInfo (#const CL_DEVICE_AVAILABLE)

deviceCompilerAvailable :: DeviceID -> Bool
deviceCompilerAvailable = deviceInfo (#const CL_DEVICE_COMPILER_AVAILABLE)

deviceName :: DeviceID -> String
deviceName = deviceInfo (#const CL_DEVICE_NAME)

deviceVendor :: DeviceID -> String
deviceVendor = deviceInfo (#const CL_DEVICE_VENDOR)

driverVersion :: DeviceID -> String
driverVersion = deviceInfo (#const CL_DRIVER_VERSION)

deviceProfile :: DeviceID -> String
deviceProfile = deviceInfo (#const CL_DEVICE_PROFILE)

deviceVersion :: DeviceID -> String
deviceVersion = deviceInfo (#const CL_DEVICE_VERSION)

deviceExtensions :: DeviceID -> String
deviceExtensions = deviceInfo (#const CL_DEVICE_EXTENSIONS)

--------

deviceQueueProperties :: DeviceID -> [CommandQueueProperty]
deviceQueueProperties dev = unsafePerformIO
        $ getFlags (clGetDeviceInfo dev (#const CL_DEVICE_QUEUE_PROPERTIES))
                            [QueueOutOfOrderExecModeEnable
                            , QueueProfilingEnable
                            ]

deviceSingleFpConfig :: DeviceID -> [DeviceFPConfig]
deviceSingleFpConfig dev = unsafePerformIO
        $ getFlags (clGetDeviceInfo dev (#const CL_DEVICE_SINGLE_FP_CONFIG))
                            [ FPDenorm
                            , FPInfNan
                            , FPRoundToNearest
                            , FPRoundToZero
                            ]

{-
deviceExecutionCapabilities :: DeviceID -> CLDeviceExecutionCapabilites
deviceExecutionCapabilities = deviceInfo (#const CL_DEVICE_EXECUTION_CAPABILITIES)

deviceLocalMemType :: DeviceID -> CLDeviceLocalMemType
deviceLocalMemType = deviceInfo (#const CL_DEVICE_LOCAL_MEM_TYPE)

deviceGlobalMemCacheType :: DeviceID -> CLDeviceGlobabMemCacheType
deviceGlobalMemCacheType = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_TYPE)
-}

devicePlatform :: DeviceID -> PlatformID
devicePlatform = PlatformID . castPtr . deviceInfo (#const CL_DEVICE_PLATFORM)
