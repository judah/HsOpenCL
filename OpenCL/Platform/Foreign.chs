module OpenCL.Platform.Foreign where

#include <OpenCL/OpenCL.h>


import OpenCL.Internal.C2HS
import OpenCL.Error
import OpenCL.Internal.Types

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
    DeviceTypeAll = CL_DEVICE_TYPE_ALL
};
#endc
{#enum CLDeviceType as DeviceType {} deriving (Eq,Show)#}

-- since CL_DEVICE_TYPE_ALL causes an overflow:
deviceTypeEnum :: DeviceType -> CULLong 
deviceTypeEnum DeviceTypeAll = 0xFFFFFFFF
deviceTypeEnum t = cEnum t

{#fun unsafe clGetDeviceIDs as clGetDeviceIDs
  { id `Ptr ()' -- to be ignored
  , deviceTypeEnum `DeviceType'
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

