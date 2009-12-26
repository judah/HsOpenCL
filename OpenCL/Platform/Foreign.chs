module OpenCL.Platform.Foreign where

#include <OpenCL/OpenCL.h>


import OpenCL.Helpers.C2HS
import OpenCL.Error
import OpenCL.Helpers.Types

import Control.Applicative

#c
enum CLDeviceType {
    DeviceTypeCPU = CL_DEVICE_TYPE_CPU,
    DeviceTypeGPU = CL_DEVICE_TYPE_GPU,
    DeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    DeviceTypeDefault = CL_DEVICE_TYPE_DEFAULT,
    DeviceTypeAll = CL_DEVICE_TYPE_ALL
};
#endc
{#enum CLDeviceType {} deriving (Eq,Show)#}

-- since CL_DEVICE_TYPE_ALL causes an overflow:
deviceTypeEnum :: CLDeviceType -> CULLong 
deviceTypeEnum DeviceTypeAll = 0xFFFFFFFF
deviceTypeEnum t = cEnum t

{#fun unsafe clGetDeviceIDs as clGetDeviceIDs
  { id `Ptr ()' -- to be ignored
  , deviceTypeEnum `CLDeviceType'
  , `Int'
  ,  castPtr `Ptr (Ptr _CLDeviceID)'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

{#fun unsafe clGetDeviceInfo
 { clDeviceIDPtr `CLDeviceID'
 , `Int'
 , `Int'
 , castPtr `Ptr a'
 , alloca- `Int' return*-
 } -> `Int' checkSuccess*-
#}

#c
enum CLDeviceFPConfig{
    CLFPDenorm = CL_FP_DENORM,
    CLFPInfNan = CL_FP_INF_NAN,
    CLFPRoundToNearest = CL_FP_ROUND_TO_NEAREST,
    CLFPRoundToZero = CL_FP_ROUND_TO_ZERO
};
#endc
{#enum CLDeviceFPConfig {} deriving (Eq,Show)#}

