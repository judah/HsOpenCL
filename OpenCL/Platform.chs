module OpenCL.Platform where

#include <OpenCL/OpenCL.h>
#include "platform_helpers.h"

import Foreign
import Foreign.C
import C2HS
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
{#enum CLDeviceType {} deriving (Eq)#}

{-
newtype CLDeviceID = CLDeviceID (ForeignPtr (Ptr ()))
                        deriving Show
clDeviceIDPtr :: CLDeviceID -> Ptr (Ptr ())
clDeviceIDPtr (CLDeviceID p) = castPtr p

newCLDeviceID :: IO CLDeviceID
newCLDeviceID = CLDeviceID <$> mallocBytes {#sizeof cl_device_id#}
-}

{#pointer *cl_device_id as CLDeviceID foreign newtype #}
newCLDeviceID :: IO CLDeviceID
newCLDeviceID = CLDeviceID <$> mallocForeignPtrBytes {#sizeof cl_device_id#}

-- cl_int clGetDeviceIDs (cl_platform_id platform, cl_device_type device_type, cl_uint num_entries, cl_device_id *devices, cl_uint *num_devices)
{#fun unsafe clGetDeviceIDs as clGetDeviceIDs
  { id `Ptr ()' -- to be ignored
  , cEnum `CLDeviceType'
  , `Int'
  , withCLDeviceID* `CLDeviceID'
  , id `Ptr CUInt' -- To be ignored
  } -> `Int'
#}

-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum



#c
enum CLDeviceInfo {
    CLDeviceName = CL_DEVICE_NAME,
    CLDeviceVecotr=  CL_DEVICE_VENDOR
};
#endc
{#enum CLDeviceInfo {}#}

{#fun unsafe clGetDeviceInfoPtr 
 { withCLDeviceID* `CLDeviceID'
 , cEnum `CLDeviceInfo'
 , `Int'
 , castPtr `Ptr a'
 , alloca- `Int' peekEnum*
 } -> `Int'
#}

strLen = 1024
clDeviceName :: CLDeviceID -> IO String
clDeviceName devID = allocaBytes strLen $ \c_str -> do
    (res,len) <- clGetDeviceInfoPtr devID CLDeviceName strLen c_str
    print ("result was:",res)
    peekCString c_str
