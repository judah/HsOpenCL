module OpenCL.Platform(
            CLDeviceType(..),
            CLDeviceID,
            getDeviceID,
            clDeviceName,
            clDeviceVendor,
            ) where

#include <OpenCL/OpenCL.h>
#include "opencl_wrappers.h"

import Control.Applicative

import OpenCL.Helpers.C2HS
import OpenCL.Error
{#import OpenCL.Helpers.Types#}

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

-- cl_int clGetDeviceIDs (cl_platform_id platform, cl_device_type device_type, cl_uint num_entries, cl_device_id *devices, cl_uint *num_devices)
{#fun unsafe clGetDeviceIDs as clGetDeviceIDs
  { id `Ptr ()' -- to be ignored
  , cEnum `CLDeviceType'
  , `Int'
  ,  castPtr `Ptr (Ptr _CLDeviceID)'
  , id `Ptr CUInt' -- To be ignored
  } -> `Int' checkSuccess*-
#}


-- TODO: get several at once?
getDeviceID :: CLDeviceType -> IO CLDeviceID
getDeviceID dtype = alloca $ \p -> do
    clGetDeviceIDs nullPtr dtype 1 p nullPtr
    CLDeviceID <$> peek p


#c
enum CLDeviceInfo {
    CLDeviceName = CL_DEVICE_NAME,
    CLDeviceVendor =  CL_DEVICE_VENDOR
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

stringInfo :: CLDeviceInfo -> CLDeviceID -> IO String
stringInfo devInfo devID = allocaBytes infoStrLen $ \c_str -> do
    clGetDeviceInfo devID devInfo infoStrLen c_str
    peekCString c_str
    
infoStrLen :: Int
infoStrLen = 1024

-- Next up: figure out an error handling scheme.
-- (throw exception, perhaps using *- in output)
