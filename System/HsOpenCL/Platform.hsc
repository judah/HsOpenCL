module System.HsOpenCL.Platform(
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
            deviceTypeAll,
            DeviceID,
            getDeviceID,
            getDeviceIDs,
            -- * Contexts
            Context,
            createContext,
            createContextFromType,
            contextDevices,
            setNotifier,
            Notifier,
            -- * Device properties
            Size,
            ClULong, 
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
            deviceGlobalMemCacheType,
            DeviceGlobalMemCacheType(..),
            deviceGlobalMemCachelineSize,
            deviceGlobalMemCacheSize,
            deviceGlobalMemSize,
            deviceMaxConstantBufferSize,
            deviceMaxConstantArgs,
            deviceLocalMemType,
            DeviceLocalMemType(..),
            deviceLocalMemSize,
            deviceErrorCorrectionSupport,
            deviceProfilingTimerResolution,
            deviceEndianLittle,
            deviceAvailable,
            deviceCompilerAvailable,
            deviceExecutionCapabilities,
            DeviceExecutionCapability(..),
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
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.ByteString (ByteString, packCStringLen)

import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.Internal.Types
import System.HsOpenCL.Platform.Foreign

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



getDeviceID :: [DeviceType] -> IO DeviceID
getDeviceID dtype = alloca $ \p -> do
    r <- clGetDeviceIDs nullPtr dtype 1 p
    -- Double-check return; in particular, OS X 10.6.2 doesn't give the correct error.
    if r < 1
        then throw CLDeviceNotFound
        else DeviceID <$> peek (castPtr p)

getDeviceIDs :: [DeviceType] -> Maybe PlatformID -> IO [DeviceID]
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
type ClUInt = #type cl_uint
type ClULong = #type cl_ulong

-- Don't worrry about overflow; the spec says that CL_DEVICE_TYPE_ALL
-- won't be returned.
deviceType :: DeviceID -> [DeviceType]
deviceType d = getPureFlags (clGetDeviceInfo d (#const CL_DEVICE_TYPE))
                        [DeviceTypeCPU, DeviceTypeGPU
                        , DeviceTypeAccelerator, DeviceTypeDefault
                        ]

deviceVendorId :: DeviceID -> ClUInt
deviceVendorId = deviceInfo (#const CL_DEVICE_VENDOR_ID)

deviceMaxComputeUnits :: DeviceID -> ClUInt
deviceMaxComputeUnits = deviceInfo (#const CL_DEVICE_MAX_COMPUTE_UNITS)

deviceMaxWorkItemDimensions :: DeviceID -> CUInt
deviceMaxWorkItemDimensions = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS)

deviceMaxWorkGroupSize :: DeviceID -> Size
deviceMaxWorkGroupSize = deviceInfo (#const CL_DEVICE_MAX_WORK_GROUP_SIZE)

deviceMaxWorkItemSizes :: DeviceID -> [Size]
deviceMaxWorkItemSizes = deviceInfo (#const CL_DEVICE_MAX_WORK_ITEM_SIZES)

devicePreferredVectorWidthChar :: DeviceID -> ClUInt
devicePreferredVectorWidthChar = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR)

devicePreferredVectorWidthShort :: DeviceID -> ClUInt
devicePreferredVectorWidthShort = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT)

devicePreferredVectorWidthInt :: DeviceID -> ClUInt
devicePreferredVectorWidthInt = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT)

devicePreferredVectorWidthLong :: DeviceID -> ClUInt
devicePreferredVectorWidthLong = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG)

devicePreferredVectorWidthFloat :: DeviceID -> ClUInt
devicePreferredVectorWidthFloat = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT)

devicePreferredVectorWidthDouble :: DeviceID -> ClUInt
devicePreferredVectorWidthDouble = deviceInfo (#const CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE)

deviceMaxClockFrequency :: DeviceID -> ClUInt
deviceMaxClockFrequency = deviceInfo (#const CL_DEVICE_MAX_CLOCK_FREQUENCY)

deviceAddressBits :: DeviceID -> ClUInt
deviceAddressBits = deviceInfo (#const CL_DEVICE_ADDRESS_BITS)

deviceMaxReadImageArgs :: DeviceID -> ClUInt
deviceMaxReadImageArgs = deviceInfo (#const CL_DEVICE_MAX_READ_IMAGE_ARGS)

deviceMaxWriteImageArgs :: DeviceID -> ClUInt
deviceMaxWriteImageArgs = deviceInfo (#const CL_DEVICE_MAX_WRITE_IMAGE_ARGS)

deviceMaxMemAllocSize :: DeviceID -> CULLong
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

deviceMaxSamplers :: DeviceID -> CUInt
deviceMaxSamplers = deviceInfo (#const CL_DEVICE_MAX_SAMPLERS)

deviceMemBaseAddrAlign :: DeviceID -> ClUInt
deviceMemBaseAddrAlign = deviceInfo (#const CL_DEVICE_MEM_BASE_ADDR_ALIGN)

deviceMinDataTypeAlignSize :: DeviceID -> ClUInt
deviceMinDataTypeAlignSize = deviceInfo (#const CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE)

deviceGlobalMemCachelineSize :: DeviceID -> CInt
deviceGlobalMemCachelineSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE)

deviceGlobalMemCacheSize :: DeviceID -> ClULong
deviceGlobalMemCacheSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_SIZE)

deviceGlobalMemSize :: DeviceID -> ClULong
deviceGlobalMemSize = deviceInfo (#const CL_DEVICE_GLOBAL_MEM_SIZE)

deviceMaxConstantBufferSize :: DeviceID -> ClULong
deviceMaxConstantBufferSize = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE)

deviceMaxConstantArgs :: DeviceID -> CUInt
deviceMaxConstantArgs = deviceInfo (#const CL_DEVICE_MAX_CONSTANT_ARGS)

deviceLocalMemSize :: DeviceID -> ClULong
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
deviceQueueProperties dev = getPureFlags
        (clGetDeviceInfo dev (#const CL_DEVICE_QUEUE_PROPERTIES))
            [QueueOutOfOrderExecModeEnable
            , QueueProfilingEnable
            ]

deviceSingleFpConfig :: DeviceID -> [DeviceFPConfig]
deviceSingleFpConfig dev = getPureFlags
        (clGetDeviceInfo dev (#const CL_DEVICE_SINGLE_FP_CONFIG))
            [ FPDenorm
            , FPInfNan
            , FPRoundToNearest
            , FPRoundToZero
            ]

deviceExecutionCapabilities :: DeviceID -> [DeviceExecutionCapability]
deviceExecutionCapabilities dev = getPureFlags
        (clGetDeviceInfo dev (#const CL_DEVICE_EXECUTION_CAPABILITIES))
                [ExecKernel, ExecNativeKernel]

deviceLocalMemType :: DeviceID -> DeviceLocalMemType
deviceLocalMemType = (cEnum :: (#type cl_device_local_mem_type) -> DeviceLocalMemType)
                        . deviceInfo (#const CL_DEVICE_LOCAL_MEM_TYPE)

data DeviceGlobalMemCacheType = ReadOnlyCache | ReadWriteCache
                                    deriving (Show,Eq)

deviceGlobalMemCacheType :: DeviceID -> Maybe DeviceGlobalMemCacheType
deviceGlobalMemCacheType d
    = case cEnum (deviceInfo (#const CL_DEVICE_GLOBAL_MEM_CACHE_TYPE) d
                    :: (#type cl_device_mem_cache_type)) of
        CLNone -> Nothing
        CLReadOnlyCache -> Just ReadOnlyCache
        CLReadWriteCache -> Just ReadWriteCache

devicePlatform :: DeviceID -> PlatformID
devicePlatform = PlatformID . castPtr . deviceInfo (#const CL_DEVICE_PLATFORM)

---------------------------
-- Contexts

createContext :: [DeviceID] -> IO Context
createContext devices = withNotifier $ clCreateContext nullPtr devices

foreign import ccall "wrapper" mkNotifier :: CNotifier -> IO (FunPtr CNotifier)

createContextFromType :: [DeviceType] -> IO Context
createContextFromType dtype = withNotifier $ clCreateContextFromType nullPtr dtype

withNotifier :: (FunPtr CNotifier -> Ptr () -> IO a) -> IO a
withNotifier f = do
    c_notify <- mkNotifier notify
    f c_notify nullPtr
  where
    notify c_errinfo c_private_info size _ = do
        errinfo <- peekCString c_errinfo
        privateInfo <- packCStringLen (castPtr c_private_info :: Ptr CChar,
                                        fromEnum size)
        notifierFunc <- readMVar notifyMVar
        notifierFunc errinfo privateInfo

contextDevices :: Context -> [DeviceID]
contextDevices context = map DeviceID
        $ getPureProp (clGetContextInfo context CLContextDevices)

type Notifier = String -- ^ An error string.
                -> ByteString -- ^ Binary data which can be used to log
                                -- additional information (implementation-dependent).
                -> IO ()

-- I apologize for this hack.
{-# NOINLINE notifyMVar #-}
notifyMVar :: MVar Notifier
notifyMVar = unsafePerformIO $ newMVar $ \_ _ -> return ()

-- | Set an action to be run when an error occurs in a context.
-- 
-- The default is for no action to be run.
setNotifier :: MonadIO m => Notifier -> m ()
setNotifier nf = liftIO $ swapMVar notifyMVar nf >> return ()
