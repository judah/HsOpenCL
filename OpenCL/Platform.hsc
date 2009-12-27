module OpenCL.Platform(
            -- * Getting devices
            DeviceType(..),
            DeviceID,
            getDeviceID,
            getDeviceIDs,
            -- * Device properties
            Size,
            ULong, 
            clDeviceType,
            clDeviceVendorId,
            clDeviceMaxComputeUnits,
            clDeviceMaxWorkItemDimensions,
            clDeviceMaxWorkGroupSize,
            clDeviceMaxWorkItemSizes,
            clDevicePreferredVectorWidthChar,
            clDevicePreferredVectorWidthShort,
            clDevicePreferredVectorWidthInt,
            clDevicePreferredVectorWidthLong,
            clDevicePreferredVectorWidthFloat,
            clDevicePreferredVectorWidthDouble,
            clDeviceMaxClockFrequency,
            clDeviceAddressBits,
            clDeviceMaxReadImageArgs,
            clDeviceMaxWriteImageArgs,
            clDeviceMaxMemAllocSize,
            clDeviceImage2dMaxWidth,
            clDeviceImage2dMaxHeight,
            clDeviceImage3dMaxWidth,
            clDeviceImage3dMaxHeight,
            clDeviceImage3dMaxDepth,
            clDeviceImageSupport,
            clDeviceMaxParameterSize,
            clDeviceMaxSamplers,
            clDeviceMemBaseAddrAlign,
            clDeviceMinDataTypeAlignSize,
            clDeviceSingleFpConfig,
            DeviceFPConfig(..),
            -- clDeviceGlobalMemCacheType,
            clDeviceGlobalMemCachelineSize,
            clDeviceGlobalMemCacheSize,
            clDeviceGlobalMemSize,
            clDeviceMaxConstantBufferSize,
            clDeviceMaxConstantArgs,
            -- clDeviceLocalMemType,
            clDeviceLocalMemSize,
            clDeviceErrorCorrectionSupport,
            clDeviceProfilingTimerResolution,
            clDeviceEndianLittle,
            clDeviceAvailable,
            clDeviceCompilerAvailable,
            -- clDeviceExecutionCapabilities,
            clDeviceQueueProperties,
            clDeviceName,
            clDeviceVendor,
            clDriverVersion,
            clDeviceProfile,
            clDeviceVersion,
            clDeviceExtensions,
            -- clDevicePlatform,
            ) where

#include <OpenCL/OpenCL.h>

import Control.Applicative
import Data.Bits

import OpenCL.Helpers.C2HS
import OpenCL.Error
import OpenCL.Helpers.Types
import OpenCL.Platform.Foreign
import OpenCL.CommandQueue (CLCommandQueueProperty(..))

-- TODO: platforms

getDeviceID :: DeviceType -> IO DeviceID
getDeviceID dtype = alloca $ \p -> do
    clGetDeviceIDs nullPtr dtype 1 p
    DeviceID <$> peek p

getDeviceIDs :: DeviceType -> IO [DeviceID]
getDeviceIDs dtype = do
    -- First, query for the total number:
    n <- clGetDeviceIDs nullPtr dtype 0 nullPtr
    -- Now, get this list of all devices:
    allocaArray n $ \p -> do
    n' <- clGetDeviceIDs nullPtr dtype n p
    peekArray n' p >>= return . map DeviceID

-- TODO: Orphan instance...
instance Show DeviceID where
    show dev = "<" ++ clDeviceVendor dev ++ " " ++ clDeviceName dev ++ ">"
---------------

deviceInfo :: Property a => Int -> DeviceID -> a
deviceInfo prop dev = getPureProp (clGetDeviceInfo dev prop)

type Size = #type size_t
type ULong = #type cl_ulong

-- Don't worrry about overflow; the spec says that CL_DEVICE_TYPE_ALL
-- won't be returned.
clDeviceType :: DeviceID -> [DeviceType]
clDeviceType d = unsafePerformIO $ getFlags
                        (clGetDeviceInfo d (#const CL_DEVICE_TYPE))
                        [DeviceTypeCPU, DeviceTypeGPU
                        , DeviceTypeAccelerator, DeviceTypeDefault
                        ]

clDeviceVendorId :: DeviceID -> Int
clDeviceVendorId = deviceInfo (#const CL_DEVICE_VENDOR_ID)

clDeviceMaxComputeUnits :: DeviceID -> Int
clDeviceMaxComputeUnits = deviceInfo (#const CL_DEVICE_MAX_COMPUTE_UNITS)

clDeviceMaxWorkItemDimensions :: DeviceID -> Int
clDeviceMaxWorkItemDimensions = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS)

clDeviceMaxWorkGroupSize :: DeviceID -> Size
clDeviceMaxWorkGroupSize = deviceInfo (#const CL_DEVICE_MAX_WORK_GROUP_SIZE)

clDeviceMaxWorkItemSizes :: DeviceID -> [Size]
clDeviceMaxWorkItemSizes = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_SIZES)

clDevicePreferredVectorWidthChar :: DeviceID -> Int
clDevicePreferredVectorWidthChar = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR)

clDevicePreferredVectorWidthShort :: DeviceID -> Int
clDevicePreferredVectorWidthShort = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT)

clDevicePreferredVectorWidthInt :: DeviceID -> Int
clDevicePreferredVectorWidthInt = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT)

clDevicePreferredVectorWidthLong :: DeviceID -> Int
clDevicePreferredVectorWidthLong = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG)

clDevicePreferredVectorWidthFloat :: DeviceID -> Int
clDevicePreferredVectorWidthFloat = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT)

clDevicePreferredVectorWidthDouble :: DeviceID -> Int
clDevicePreferredVectorWidthDouble = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE)

clDeviceMaxClockFrequency :: DeviceID -> Int
clDeviceMaxClockFrequency = deviceInfo (#const CL_DEVICE_MAX_CLOCK_FREQUENCY)

clDeviceAddressBits :: DeviceID -> Int
clDeviceAddressBits = deviceInfo (#const CL_DEVICE_ADDRESS_BITS)

clDeviceMaxReadImageArgs :: DeviceID -> Int
clDeviceMaxReadImageArgs = deviceInfo (#const CL_DEVICE_MAX_READ_IMAGE_ARGS)

clDeviceMaxWriteImageArgs :: DeviceID -> Int
clDeviceMaxWriteImageArgs = deviceInfo (#const CL_DEVICE_MAX_WRITE_IMAGE_ARGS)

clDeviceMaxMemAllocSize :: DeviceID -> ULong
clDeviceMaxMemAllocSize = deviceInfo (#const CL_DEVICE_MAX_MEM_ALLOC_SIZE)

clDeviceImage2dMaxWidth :: DeviceID -> Size
clDeviceImage2dMaxWidth = deviceInfo (#const CL_DEVICE_IMAGE2D_MAX_WIDTH)

clDeviceImage2dMaxHeight :: DeviceID -> Size
clDeviceImage2dMaxHeight = deviceInfo (#const CL_DEVICE_IMAGE2D_MAX_HEIGHT)

clDeviceImage3dMaxWidth :: DeviceID -> Size
clDeviceImage3dMaxWidth = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_WIDTH)

clDeviceImage3dMaxHeight :: DeviceID -> Size
clDeviceImage3dMaxHeight = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_HEIGHT)

clDeviceImage3dMaxDepth :: DeviceID -> Size
clDeviceImage3dMaxDepth = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_DEPTH)

clDeviceImageSupport :: DeviceID -> Bool
clDeviceImageSupport = deviceInfo (#const CL_DEVICE_IMAGE_SUPPORT)

clDeviceMaxParameterSize :: DeviceID -> Size
clDeviceMaxParameterSize = deviceInfo (#const CL_DEVICE_MAX_PARAMETER_SIZE)

clDeviceMaxSamplers :: DeviceID -> Int
clDeviceMaxSamplers = deviceInfo (#const CL_DEVICE_MAX_SAMPLERS)

clDeviceMemBaseAddrAlign :: DeviceID -> Int
clDeviceMemBaseAddrAlign = deviceInfo (#const CL_DEVICE_MEM_BASE_ADDR_ALIGN)

clDeviceMinDataTypeAlignSize :: DeviceID -> Int
clDeviceMinDataTypeAlignSize = deviceInfo (#const CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE)

clDeviceGlobalMemCachelineSize :: DeviceID -> Int
clDeviceGlobalMemCachelineSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE)

clDeviceGlobalMemCacheSize :: DeviceID -> ULong
clDeviceGlobalMemCacheSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_SIZE)

clDeviceGlobalMemSize :: DeviceID -> ULong
clDeviceGlobalMemSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_SIZE)

clDeviceMaxConstantBufferSize :: DeviceID -> ULong
clDeviceMaxConstantBufferSize = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE)

clDeviceMaxConstantArgs :: DeviceID -> Int
clDeviceMaxConstantArgs = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_ARGS)

clDeviceLocalMemSize :: DeviceID -> ULong
clDeviceLocalMemSize = deviceInfo (#const CL_DEVICE_LOCAL_MEM_SIZE)

clDeviceErrorCorrectionSupport :: DeviceID -> Bool
clDeviceErrorCorrectionSupport = deviceInfo (#const CL_DEVICE_ERROR_CORRECTION_SUPPORT)

clDeviceProfilingTimerResolution :: DeviceID -> Size
clDeviceProfilingTimerResolution = deviceInfo (#const CL_DEVICE_PROFILING_TIMER_RESOLUTION)

clDeviceEndianLittle :: DeviceID -> Bool
clDeviceEndianLittle = deviceInfo (#const CL_DEVICE_ENDIAN_LITTLE)

clDeviceAvailable :: DeviceID -> Bool
clDeviceAvailable = deviceInfo (#const CL_DEVICE_AVAILABLE)

clDeviceCompilerAvailable :: DeviceID -> Bool
clDeviceCompilerAvailable = deviceInfo (#const CL_DEVICE_COMPILER_AVAILABLE)

clDeviceName :: DeviceID -> String
clDeviceName = deviceInfo (#const CL_DEVICE_NAME)

clDeviceVendor :: DeviceID -> String
clDeviceVendor = deviceInfo (#const CL_DEVICE_VENDOR)

clDriverVersion :: DeviceID -> String
clDriverVersion = deviceInfo (#const CL_DRIVER_VERSION)

clDeviceProfile :: DeviceID -> String
clDeviceProfile = deviceInfo (#const CL_DEVICE_PROFILE)

clDeviceVersion :: DeviceID -> String
clDeviceVersion = deviceInfo (#const CL_DEVICE_VERSION)

clDeviceExtensions :: DeviceID -> String
clDeviceExtensions = deviceInfo (#const CL_DEVICE_EXTENSIONS)

--------

clDeviceQueueProperties :: DeviceID -> [CLCommandQueueProperty]
clDeviceQueueProperties dev = unsafePerformIO
        $ getFlags (clGetDeviceInfo dev (#const CL_DEVICE_QUEUE_PROPERTIES))
                            [CLQueueOutOfOrderExecModeEnable
                            , CLQueueProfilingEnable
                            ]

clDeviceSingleFpConfig :: DeviceID -> [DeviceFPConfig]
clDeviceSingleFpConfig dev = unsafePerformIO
        $ getFlags (clGetDeviceInfo dev (#const CL_DEVICE_SINGLE_FP_CONFIG))
                            [ FPDenorm
                            , FPInfNan
                            , FPRoundToNearest
                            , FPRoundToZero
                            ]

{-
clDeviceExecutionCapabilities :: DeviceID -> CLDeviceExecutionCapabilites
clDeviceExecutionCapabilities = deviceInfo (#const CL_DEVICE_EXECUTION_CAPABILITIES)

clDeviceLocalMemType :: DeviceID -> CLDeviceLocalMemType
clDeviceLocalMemType = deviceInfo (#const CL_DEVICE_LOCAL_MEM_TYPE)

clDeviceGlobalMemCacheType :: DeviceID -> CLDeviceGlobabMemCacheType
clDeviceGlobalMemCacheType = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_TYPE)

clDevicePlatform :: DeviceID -> CLPlatformID
clDevicePlatform = deviceInfo (#const CL_DEVICE_PLATFORM)
-}
