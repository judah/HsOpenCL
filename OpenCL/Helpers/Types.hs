module OpenCL.Helpers.Types where

import Foreign

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

newtype CLContext = CLContext (Ptr CLContext)
clContextPtr :: CLContext -> Ptr ()
clContextPtr (CLContext p) = castPtr p




