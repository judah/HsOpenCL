module OpenCL.Helpers.Types where

import Foreign
import Control.Applicative

-- I'm assuming that types like cl_device_id are actually pointers,
-- so they can be passed around by the FFI.
-- This is true on Apple systems, but I'm not sure how portable it is.


-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum

newData :: (ForeignPtr a_ -> a) -> Releaser a_
                    -> (Ptr () -> IO a)
newData construct release p = construct <$> newForeignPtr release (castPtr p)


-- Note that unlike the others, CLDeviceID has no way to be released
-- so it's just a pointer.
data CLDeviceID_
newtype CLDeviceID = CLDeviceID (Ptr CLDeviceID_)
clDeviceIDPtr :: CLDeviceID -> Ptr ()
clDeviceIDPtr (CLDeviceID p) = castPtr p

data CLContext_
newtype CLContext = CLContext (ForeignPtr CLContext_)
withCLContext :: CLContext -> (Ptr () -> IO a) -> IO a
withCLContext (CLContext p) f = withForeignPtr p $ f . castPtr

type Releaser a_ = FunPtr (Ptr a_ -> IO ())

data CLCommandQueue_
newtype CLCommandQueue = CLCommandQueue (ForeignPtr CLCommandQueue_)
withCLCommandQueue :: CLCommandQueue -> (Ptr () -> IO a) -> IO a
withCLCommandQueue (CLCommandQueue p) f = withForeignPtr p $ f . castPtr

data CLProgram_
newtype CLProgram = CLProgram (ForeignPtr CLProgram_)
withCLProgram :: CLProgram -> (Ptr () -> IO a) -> IO a
withCLProgram (CLProgram p) f = withForeignPtr p $ f . castPtr

data CLMem_
newtype CLMem = CLMem (ForeignPtr CLMem_)
withCLMem :: CLMem -> (Ptr () -> IO a) -> IO a
withCLMem (CLMem p) f = withForeignPtr p $ f . castPtr

data CLKernel_
newtype CLKernel = CLKernel (ForeignPtr CLKernel_)
withCLKernel :: CLKernel -> (Ptr () -> IO a) -> IO a
withCLKernel (CLKernel p) f = withForeignPtr p $ f . castPtr


