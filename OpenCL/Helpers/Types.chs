module OpenCL.Helpers.Types where

#include <OpenCL/OpenCL.h>

import Foreign
import Foreign.C
import Control.Applicative

import OpenCL.Error

-- I'm assuming that types like cl_device_id are actually pointers,
-- so they can be passed around by the FFI.
-- This is true on Apple systems, but I'm not sure how portable it is.

type Releaser a_ = FunPtr (Ptr a_ -> IO ())

-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum

newData :: (ForeignPtr a_ -> a) -> Releaser a_
                    -> (Ptr () -> IO a)
newData construct release p = construct <$> newForeignPtr release (castPtr p)


-- Note that unlike the others, CLDeviceID has no way to be released
-- so it's just a pointer.
data CLDeviceID_
newtype CLDeviceID = CLDeviceID {_clDeviceIDPtr :: Ptr CLDeviceID_}
clDeviceIDPtr :: CLDeviceID -> Ptr ()
clDeviceIDPtr (CLDeviceID p) = castPtr p

------------
data CLContext_
newtype CLContext = CLContext (ForeignPtr CLContext_)
withCLContext :: CLContext -> (Ptr () -> IO a) -> IO a
withCLContext (CLContext p) f = withForeignPtr p $ f . castPtr

newCLContext = newData CLContext clReleaseContext
foreign import ccall "&" clReleaseContext :: Releaser CLContext_

{#fun clRetainContext
  { id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

-- Note: be careful of races if the child is a ForeignPtr.  (See clQueueContext).
retainedCLContext :: Ptr () -> IO CLContext
retainedCLContext p = clRetainContext p >> newCLContext p

-- Being careful of race conditions:
-- The foreignptr points back to the context, and if it's GC'd
-- it could cause the context to be released prematurely.
-- So, make sure the ForeignPtr stays alive long enough for us to retain
-- the context.
retainedContextInfo :: ForeignPtr a -> Ptr () -> IO CLContext
retainedContextInfo child p = withForeignPtr child $ \_ -> do
    clRetainContext p
    newCLContext p
------------

data CLCommandQueue_
newtype CLCommandQueue = CLCommandQueue (ForeignPtr CLCommandQueue_)
withCLCommandQueue :: CLCommandQueue -> (Ptr () -> IO a) -> IO a
withCLCommandQueue (CLCommandQueue p) f = withForeignPtr p $ f . castPtr

data CLProgram_
newtype CLProgram = CLProgram (ForeignPtr CLProgram_)
withCLProgram :: CLProgram -> (Ptr () -> IO a) -> IO a
withCLProgram (CLProgram p) f = withForeignPtr p $ f . castPtr

-- Note: cl_mem's aren't retained when they're set as kernel arguments.
-- Rather, they're only retained while the kernel is running, and released
-- once it's finished.
-- So if we tried storing them in a ForeignPtr like the other types,
-- we could get the following race:
-- 1. Create CLMem for an input argument
-- 2. Call OpenCL to fill it with values
-- 3. Set it as a kernel arg
-- 4. Haskell GC
-- 5. Run kernel
-- If the CLMem is never used after step 5, it will be released prematurely
-- in step 4.  
data CLMem_
newtype CLMem a = CLMem (Ptr CLMem_)
withCLMem :: CLMem a -> (Ptr () -> IO b) -> IO b
withCLMem (CLMem p) f = f $ castPtr p

data CLKernel_
newtype CLKernel = CLKernel (ForeignPtr CLKernel_)
withCLKernel :: CLKernel -> (Ptr () -> IO a) -> IO a
withCLKernel (CLKernel p) f = withForeignPtr p $ f . castPtr


