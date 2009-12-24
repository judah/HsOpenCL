module OpenCL.Helpers.Types where

import Foreign
#include <OpenCL/OpenCL.h>
{#pointer *cl_device_id as CLDeviceID foreign newtype #}

-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum
