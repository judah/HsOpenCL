module OpenCL.Helpers.Types where

import Foreign
#include <OpenCL/OpenCL.h>

-- TODO: Change this:
-- currently we keep around a device_id*, but really we want a device_id
-- since it itself is actually a pointer.
{#pointer *cl_device_id as CLDeviceID foreign newtype #}

-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum

-- OK, trying contexts as just pointer types:
-- TODO: foreign pointer plus release when done.
newtype CLContext = CLContext (Ptr CLContext)
newtype CLCommandQueue = CLCommandQueue (Ptr CLCommandQueue)
