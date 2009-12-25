module OpenCL.Platform(
            CLDeviceType(..),
            CLDeviceID,
            getDeviceID,
            getDeviceIDs,
            clDeviceName,
            clDeviceVendor,
            clDriverVersion,
            clDeviceProfile,
            clDeviceVersion,
            clDeviceExtensions,
            ) where

#include <OpenCL/OpenCL.h>
#include "opencl_wrappers.h"

import Control.Applicative

import OpenCL.Helpers.C2HS
import OpenCL.Error
import OpenCL.Helpers.Types



-- TODO: platforms



#c
enum CLDeviceType {
    DeviceTypeCPU = CL_DEVICE_TYPE_CPU,
    DeviceTypeGPU = CL_DEVICE_TYPE_GPU,
    DeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    DeviceTypeDefault = CL_DEVICE_TYPE_DEFAULT,
    DeviceTypeAll = CL_DEVICE_TYPE_ALL
};
#endc
{#enum CLDeviceType {} #}

-- since CL_DEVICE_TYPE_ALL causes an overflow:
deviceTypeEnum :: CLDeviceType -> CULLong 
deviceTypeEnum DeviceTypeAll = 0xFFFFFFFF
deviceTypeEnum t = cEnum t


-- cl_int clGetDeviceIDs (cl_platform_id platform, cl_device_type device_type, cl_uint num_entries, cl_device_id *devices, cl_uint *num_devices)
{#fun unsafe clGetDeviceIDs as clGetDeviceIDs
  { id `Ptr ()' -- to be ignored
  , deviceTypeEnum `CLDeviceType'
  , `Int'
  ,  castPtr `Ptr (Ptr _CLDeviceID)'
  , alloca- `Int' peekIntConv* -- To be ignored
  } -> `Int' checkSuccess*-
#}


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

-- TODO: is it efficient to use this giant enum?
#c
enum CLDeviceInfo {
    CLDeviceType = CL_DEVICE_TYPE,
    CLDeviceVendorId = CL_DEVICE_VENDOR_ID,
    CLDeviceMaxComputeUnits = CL_DEVICE_MAX_COMPUTE_UNITS,
    CLDeviceMaxWorkItemDimensions = CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
    CLDeviceMaxWorkGroupSize = CL_DEVICE_MAX_WORK_GROUP_SIZE,
    CLDeviceMaxWorkItemSizes = CL_DEVICE_MAX_WORK_ITEM_SIZES,
    CLDevicePreferredVectorWidthChar = CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
    CLDevicePreferredVectorWidthShort = CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
    CLDevicePreferredVectorWidthInt = CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
    CLDevicePreferredVectorWidthLong = CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
    CLDevicePreferredVectorWidthFloat = CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
    CLDevicePreferredVectorWidthDouble = CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
    CLDeviceMaxClockFrequency = CL_DEVICE_MAX_CLOCK_FREQUENCY,
    CLDeviceAddressBits = CL_DEVICE_ADDRESS_BITS,
    CLDeviceMaxReadImageArgs = CL_DEVICE_MAX_READ_IMAGE_ARGS,
    CLDeviceMaxWriteImageArgs = CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
    CLDeviceMaxMemAllocSize = CL_DEVICE_MAX_MEM_ALLOC_SIZE,
    CLDeviceImage2dMaxWidth = CL_DEVICE_IMAGE2D_MAX_WIDTH,
    CLDeviceImage2dMaxHeight = CL_DEVICE_IMAGE2D_MAX_HEIGHT,
    CLDeviceImage3dMaxWidth = CL_DEVICE_IMAGE3D_MAX_WIDTH,
    CLDeviceImage3dMaxHeight = CL_DEVICE_IMAGE3D_MAX_HEIGHT,
    CLDeviceImage3dMaxDepth = CL_DEVICE_IMAGE3D_MAX_DEPTH,
    CLDeviceImageSupport = CL_DEVICE_IMAGE_SUPPORT,
    CLDeviceMaxParameterSize = CL_DEVICE_MAX_PARAMETER_SIZE,
    CLDeviceMaxSamplers = CL_DEVICE_MAX_SAMPLERS,
    CLDeviceMemBaseAddrAlign = CL_DEVICE_MEM_BASE_ADDR_ALIGN,
    CLDeviceMinDataTypeAlignSize = CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
    CLDeviceSingleFpConfig = CL_DEVICE_SINGLE_FP_CONFIG,
    CLDeviceGlobalMemCacheType = CL_DEVICE_GLOBAL_MEM_CACHE_TYPE,
    CLDeviceGlobalMemCachelineSize = CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
    CLDeviceGlobalMemCacheSize = CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
    CLDeviceGlobalMemSize = CL_DEVICE_GLOBAL_MEM_SIZE,
    CLDeviceMaxConstantBufferSize = CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
    CLDeviceMaxConstantArgs = CL_DEVICE_MAX_CONSTANT_ARGS,
    CLDeviceLocalMemType = CL_DEVICE_LOCAL_MEM_TYPE,
    CLDeviceLocalMemSize = CL_DEVICE_LOCAL_MEM_SIZE,
    CLDeviceErrorCorrectionSupport = CL_DEVICE_ERROR_CORRECTION_SUPPORT,
    CLDeviceProfilingTimerResolution = CL_DEVICE_PROFILING_TIMER_RESOLUTION,
    CLDeviceEndianLittle = CL_DEVICE_ENDIAN_LITTLE,
    CLDeviceAvailable = CL_DEVICE_AVAILABLE,
    CLDeviceCompilerAvailable = CL_DEVICE_COMPILER_AVAILABLE,
    CLDeviceExecutionCapabilities = CL_DEVICE_EXECUTION_CAPABILITIES,
    CLDeviceQueueProperties = CL_DEVICE_QUEUE_PROPERTIES,
    CLDeviceName = CL_DEVICE_NAME,
    CLDeviceVendor = CL_DEVICE_VENDOR,
    CLDriverVersion = CL_DRIVER_VERSION,
    CLDeviceProfile = CL_DEVICE_PROFILE,
    CLDeviceVersion = CL_DEVICE_VERSION,
    CLDeviceExtensions = CL_DEVICE_EXTENSIONS,
    CLDevicePlatform = CL_DEVICE_PLATFORM
};
#endc
{#enum CLDeviceInfo {}#}

{#fun unsafe clGetDeviceInfo
 { clDeviceIDPtr `CLDeviceID'
 , cEnum `CLDeviceInfo'
 , `Int'
 , castPtr `Ptr a'
 , alloca- `Int' return*-
 } -> `Int' checkSuccess*-
#}

-- TODO: can these be pure?
clDeviceName :: CLDeviceID -> IO String
clDeviceName = stringInfo CLDeviceName

clDeviceVendor :: CLDeviceID -> IO String
clDeviceVendor = stringInfo CLDeviceVendor

clDriverVersion :: CLDeviceID -> IO String
clDriverVersion = stringInfo CLDriverVersion

clDeviceProfile :: CLDeviceID -> IO String
clDeviceProfile = stringInfo CLDeviceProfile

clDeviceVersion :: CLDeviceID -> IO String
clDeviceVersion = stringInfo CLDeviceVersion

clDeviceExtensions :: CLDeviceID -> IO String
clDeviceExtensions = stringInfo CLDeviceExtensions



stringInfo :: CLDeviceInfo -> CLDeviceID -> IO String
stringInfo devInfo devID = allocaBytes infoStrLen $ \c_str -> do
    clGetDeviceInfo devID devInfo infoStrLen c_str
    peekCString c_str
    
infoStrLen :: Int
infoStrLen = 1024

-- Next up: figure out an error handling scheme.
-- (throw exception, perhaps using *- in output)
