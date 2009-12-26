module OpenCL.Context.Retainer where

#include <OpenCL/OpenCL.h>


import OpenCL.Helpers.C2HS
import OpenCL.Error
import OpenCL.Helpers.Types

newCLContext = newData CLContext clReleaseContext
foreign import ccall "&" clReleaseContext :: Releaser CLContext_

{#fun clRetainContext
  { id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

-- Being careful of race conditions:
-- The foreignptr points back to the context, and if it's GC'd
-- it could cause the context to be released prematurely.
-- So, make sure the ForeignPtr stays alive long enough for us to retain
-- the context.
retainedContextInfo :: ForeignPtr a -> Ptr () -> IO CLContext
retainedContextInfo child p = withForeignPtr child $ \_ -> do
    clRetainContext p
    newCLContext p
