module System.HsOpenCL.Platform.Foreign where

#include <OpenCL/OpenCL.h>


import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.Internal.Types

import Control.Applicative

{#fun clGetPlatformIDs
  { `Int'
  , castPtr `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

#c
enum CLPlatformInfo {
    CLPlatformProfile = CL_PLATFORM_PROFILE,
    CLPlatformVersion = CL_PLATFORM_VERSION,
    CLPlatformName = CL_PLATFORM_NAME,
    CLPlatformVendor = CL_PLATFORM_VENDOR,
    CLPlatformExtensions = CL_PLATFORM_EXTENSIONS,
};
#endc
{#enum CLPlatformInfo {} #}
{#fun clGetPlatformInfo as getPlatformInfo
  { platformIDPtr `PlatformID'
  , cEnum `CLPlatformInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

#c
enum CLDeviceType {
    DeviceTypeCPU = CL_DEVICE_TYPE_CPU,
    DeviceTypeGPU = CL_DEVICE_TYPE_GPU,
    DeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    DeviceTypeDefault = CL_DEVICE_TYPE_DEFAULT,
};
#endc
{#enum CLDeviceType as DeviceType {} deriving (Eq,Show,Read)#}

deviceTypeAll :: [DeviceType]
deviceTypeAll = [DeviceTypeCPU, DeviceTypeGPU, DeviceTypeAccelerator, DeviceTypeDefault]

{#fun unsafe clGetDeviceIDs as clGetDeviceIDs
  { id `Ptr ()' -- to be ignored
  , combineBitMasks `[DeviceType]'
  , `Int'
  ,  castPtr `Ptr (Ptr _CLDeviceID)'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

{#fun unsafe clGetDeviceInfo
 { deviceIDPtr `DeviceID'
 , `Int'
 , `Int'
 , castPtr `Ptr a'
 , alloca- `Int' peekIntConv*
 } -> `Int' checkSuccess*-
#}

#c
enum CLDeviceFPConfig{
    FPDenorm = CL_FP_DENORM,
    FPInfNan = CL_FP_INF_NAN,
    FPRoundToNearest = CL_FP_ROUND_TO_NEAREST,
    FPRoundToZero = CL_FP_ROUND_TO_ZERO
};
#endc
{#enum CLDeviceFPConfig as DeviceFPConfig {} deriving (Eq,Show)#}

#c
enum CommandQueueProperty {
    QueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
    QueueProfilingEnable = CL_QUEUE_PROFILING_ENABLE
};
#endc
{#enum CommandQueueProperty {} deriving (Show,Eq)#}

#c
enum DeviceExecutionCapability {
    ExecKernel = CL_EXEC_KERNEL,
    ExecNativeKernel = CL_EXEC_NATIVE_KERNEL,
};
#endc    
{#enum DeviceExecutionCapability {} deriving (Show,Eq)#}

#c
enum DeviceLocalMemType {
    LocalMem = CL_LOCAL,
    GlobalMem = CL_GLOBAL,
};
#endc
{#enum DeviceLocalMemType {} deriving (Show,Eq)#}

#c
enum CLDeviceGlobalMemCacheType {
    CLNone = CL_NONE,
    CLReadOnlyCache = CL_READ_ONLY_CACHE,
    CLReadWriteCache = CL_READ_WRITE_CACHE,
};
#endc
{#enum CLDeviceGlobalMemCacheType {} deriving (Eq)#}


{#fun clCreateContext as clCreateContext
  { id `Ptr CLong'
  , withDeviceIDs* `[DeviceID]'&
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `Context' newContext*
#}

{#fun clCreateContextFromType as clCreateContextFromType
  { id `Ptr CLong'
  , combineBitMasks `[DeviceType]'
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `Context' newContext*
#}

{#fun clGetContextInfo as clGetContextInfo
  { withContext* `Context'
  , cEnum `CLContextInfo' -- todo do enum
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

-- The only really interesting property is the devices, for now.
#c
enum CLContextInfo {
    CLContextReferenceCount = CL_CONTEXT_REFERENCE_COUNT,
    CLContextDevices = CL_CONTEXT_DEVICES,
    CLContextProperties = CL_CONTEXT_PROPERTIES
};
#endc
{#enum CLContextInfo {} #}

