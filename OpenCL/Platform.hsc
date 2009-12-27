module OpenCL.Platform(
            -- * Getting devices
            CLDeviceType(..),
            CLDeviceID,
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

getDeviceID :: CLDeviceType -> IO CLDeviceID
getDeviceID dtype = alloca $ \p -> do
    clGetDeviceIDs nullPtr dtype 1 p
    CLDeviceID <$> peek p

getDeviceIDs :: CLDeviceType -> IO [CLDeviceID]
getDeviceIDs dtype = do
    -- First, query for the total number:
    n <- clGetDeviceIDs nullPtr dtype 0 nullPtr
    -- Now, get this list of all devices:
    allocaArray n $ \p -> do
    n' <- clGetDeviceIDs nullPtr dtype n p
    peekArray n' p >>= return . map CLDeviceID

-- TODO: Orphan instance...
instance Show CLDeviceID where
    show dev = "<" ++ clDeviceVendor dev ++ " " ++ clDeviceName dev ++ ">"
---------------

deviceInfo :: Property a => Int -> CLDeviceID -> a
deviceInfo prop dev = getPureProp (clGetDeviceInfo dev prop)

type Size = #type size_t
type ULong = #type cl_ulong

-- Don't worrry about overflow; the spec says that CL_DEVICE_TYPE_ALL
-- won't be returned.
clDeviceType :: CLDeviceID -> [CLDeviceType]
clDeviceType d = unsafePerformIO $ getFlags
                        (clGetDeviceInfo d (#const CL_DEVICE_TYPE))
                        [DeviceTypeCPU, DeviceTypeGPU
                        , DeviceTypeAccelerator, DeviceTypeDefault
                        ]

clDeviceVendorId :: CLDeviceID -> Int
clDeviceVendorId = deviceInfo (#const CL_DEVICE_VENDOR_ID)

clDeviceMaxComputeUnits :: CLDeviceID -> Int
clDeviceMaxComputeUnits = deviceInfo (#const CL_DEVICE_MAX_COMPUTE_UNITS)

clDeviceMaxWorkItemDimensions :: CLDeviceID -> Int
clDeviceMaxWorkItemDimensions = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS)

clDeviceMaxWorkGroupSize :: CLDeviceID -> Size
clDeviceMaxWorkGroupSize = deviceInfo (#const CL_DEVICE_MAX_WORK_GROUP_SIZE)

clDeviceMaxWorkItemSizes :: CLDeviceID -> [Size]
clDeviceMaxWorkItemSizes = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_SIZES)

clDevicePreferredVectorWidthChar :: CLDeviceID -> Int
clDevicePreferredVectorWidthChar = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR)

clDevicePreferredVectorWidthShort :: CLDeviceID -> Int
clDevicePreferredVectorWidthShort = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT)

clDevicePreferredVectorWidthInt :: CLDeviceID -> Int
clDevicePreferredVectorWidthInt = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT)

clDevicePreferredVectorWidthLong :: CLDeviceID -> Int
clDevicePreferredVectorWidthLong = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG)

clDevicePreferredVectorWidthFloat :: CLDeviceID -> Int
clDevicePreferredVectorWidthFloat = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT)

clDevicePreferredVectorWidthDouble :: CLDeviceID -> Int
clDevicePreferredVectorWidthDouble = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE)

clDeviceMaxClockFrequency :: CLDeviceID -> Int
clDeviceMaxClockFrequency = deviceInfo (#const CL_DEVICE_MAX_CLOCK_FREQUENCY)

clDeviceAddressBits :: CLDeviceID -> Int
clDeviceAddressBits = deviceInfo (#const CL_DEVICE_ADDRESS_BITS)

clDeviceMaxReadImageArgs :: CLDeviceID -> Int
clDeviceMaxReadImageArgs = deviceInfo (#const CL_DEVICE_MAX_READ_IMAGE_ARGS)

clDeviceMaxWriteImageArgs :: CLDeviceID -> Int
clDeviceMaxWriteImageArgs = deviceInfo (#const CL_DEVICE_MAX_WRITE_IMAGE_ARGS)

clDeviceMaxMemAllocSize :: CLDeviceID -> ULong
clDeviceMaxMemAllocSize = deviceInfo (#const CL_DEVICE_MAX_MEM_ALLOC_SIZE)

clDeviceImage2dMaxWidth :: CLDeviceID -> Size
clDeviceImage2dMaxWidth = deviceInfo (#const CL_DEVICE_IMAGE2D_MAX_WIDTH)

clDeviceImage2dMaxHeight :: CLDeviceID -> Size
clDeviceImage2dMaxHeight = deviceInfo (#const CL_DEVICE_IMAGE2D_MAX_HEIGHT)

clDeviceImage3dMaxWidth :: CLDeviceID -> Size
clDeviceImage3dMaxWidth = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_WIDTH)

clDeviceImage3dMaxHeight :: CLDeviceID -> Size
clDeviceImage3dMaxHeight = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_HEIGHT)

clDeviceImage3dMaxDepth :: CLDeviceID -> Size
clDeviceImage3dMaxDepth = deviceInfo (#const CL_DEVICE_IMAGE3D_MAX_DEPTH)

clDeviceImageSupport :: CLDeviceID -> Bool
clDeviceImageSupport = deviceInfo (#const CL_DEVICE_IMAGE_SUPPORT)

clDeviceMaxParameterSize :: CLDeviceID -> Size
clDeviceMaxParameterSize = deviceInfo (#const CL_DEVICE_MAX_PARAMETER_SIZE)

clDeviceMaxSamplers :: CLDeviceID -> Int
clDeviceMaxSamplers = deviceInfo (#const CL_DEVICE_MAX_SAMPLERS)

clDeviceMemBaseAddrAlign :: CLDeviceID -> Int
clDeviceMemBaseAddrAlign = deviceInfo (#const CL_DEVICE_MEM_BASE_ADDR_ALIGN)

clDeviceMinDataTypeAlignSize :: CLDeviceID -> Int
clDeviceMinDataTypeAlignSize = deviceInfo (#const CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE)

clDeviceGlobalMemCachelineSize :: CLDeviceID -> Int
clDeviceGlobalMemCachelineSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE)

clDeviceGlobalMemCacheSize :: CLDeviceID -> ULong
clDeviceGlobalMemCacheSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_SIZE)

clDeviceGlobalMemSize :: CLDeviceID -> ULong
clDeviceGlobalMemSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_SIZE)

clDeviceMaxConstantBufferSize :: CLDeviceID -> ULong
clDeviceMaxConstantBufferSize = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE)

clDeviceMaxConstantArgs :: CLDeviceID -> Int
clDeviceMaxConstantArgs = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_ARGS)

clDeviceLocalMemSize :: CLDeviceID -> ULong
clDeviceLocalMemSize = deviceInfo (#const CL_DEVICE_LOCAL_MEM_SIZE)

clDeviceErrorCorrectionSupport :: CLDeviceID -> Bool
clDeviceErrorCorrectionSupport = deviceInfo (#const CL_DEVICE_ERROR_CORRECTION_SUPPORT)

clDeviceProfilingTimerResolution :: CLDeviceID -> Size
clDeviceProfilingTimerResolution = deviceInfo (#const CL_DEVICE_PROFILING_TIMER_RESOLUTION)

clDeviceEndianLittle :: CLDeviceID -> Bool
clDeviceEndianLittle = deviceInfo (#const CL_DEVICE_ENDIAN_LITTLE)

clDeviceAvailable :: CLDeviceID -> Bool
clDeviceAvailable = deviceInfo (#const CL_DEVICE_AVAILABLE)

clDeviceCompilerAvailable :: CLDeviceID -> Bool
clDeviceCompilerAvailable = deviceInfo (#const CL_DEVICE_COMPILER_AVAILABLE)

clDeviceName :: CLDeviceID -> String
clDeviceName = deviceInfo (#const CL_DEVICE_NAME)

clDeviceVendor :: CLDeviceID -> String
clDeviceVendor = deviceInfo (#const CL_DEVICE_VENDOR)

clDriverVersion :: CLDeviceID -> String
clDriverVersion = deviceInfo (#const CL_DRIVER_VERSION)

clDeviceProfile :: CLDeviceID -> String
clDeviceProfile = deviceInfo (#const CL_DEVICE_PROFILE)

clDeviceVersion :: CLDeviceID -> String
clDeviceVersion = deviceInfo (#const CL_DEVICE_VERSION)

clDeviceExtensions :: CLDeviceID -> String
clDeviceExtensions = deviceInfo (#const CL_DEVICE_EXTENSIONS)

--------

clDeviceQueueProperties :: CLDeviceID -> [CLCommandQueueProperty]
clDeviceQueueProperties dev = unsafePerformIO
        $ getFlags (clGetDeviceInfo dev (#const CL_DEVICE_QUEUE_PROPERTIES))
                            [CLQueueOutOfOrderExecModeEnable
                            , CLQueueProfilingEnable
                            ]

clDeviceSingleFpConfig :: CLDeviceID -> [CLDeviceFPConfig]
clDeviceSingleFpConfig dev = unsafePerformIO
        $ getFlags (clGetDeviceInfo dev (#const CL_DEVICE_SINGLE_FP_CONFIG))
                            [ CLFPDenorm
                            , CLFPInfNan
                            , CLFPRoundToNearest
                            , CLFPRoundToZero
                            ]

{-
clDeviceExecutionCapabilities :: CLDeviceID -> CLDeviceExecutionCapabilites
clDeviceExecutionCapabilities = deviceInfo (#const CL_DEVICE_EXECUTION_CAPABILITIES)

clDeviceLocalMemType :: CLDeviceID -> CLDeviceLocalMemType
clDeviceLocalMemType = deviceInfo (#const CL_DEVICE_LOCAL_MEM_TYPE)

clDeviceGlobalMemCacheType :: CLDeviceID -> CLDeviceGlobabMemCacheType
clDeviceGlobalMemCacheType = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_TYPE)

clDevicePlatform :: CLDeviceID -> CLPlatformID
clDevicePlatform = deviceInfo (#const CL_DEVICE_PLATFORM)
-}
