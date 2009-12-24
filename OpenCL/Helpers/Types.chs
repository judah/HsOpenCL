module OpenCL.Helpers.Types where

import Foreign
#include <OpenCL/OpenCL.h>

-- I'm assuming that types like cl_device_id are actually pointers,
-- so they can be passed around by the FFI.
-- This is true on Apple systems, but I'm not sure how portable it is.

data CLDeviceID_
newtype CLDeviceID = CLDeviceID (Ptr CLDeviceID_)

clDeviceIDPtr :: CLDeviceID -> Ptr ()
clDeviceIDPtr (CLDeviceID p) = castPtr p

-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum

-- OK, trying contexts as just pointer types:
-- TODO: foreign pointer plus release when done.
newtype CLContext = CLContext (Ptr CLContext)
newtype CLCommandQueue = CLCommandQueue (Ptr CLCommandQueue)
