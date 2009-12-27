module OpenCL.CommandQueue(
                -- * Command queues
                CLCommandQueue,
                CLCommandQueueProperties(..),
                createCommandQueue,
                clFlush,
                clFinish,
                -- * Querying info and properties
                clQueueDevice,
                clQueueContext,
                clQueueProperties,
                setCommandQueueProperty,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error

import Control.Monad

-- TODO: should this be called "Property" in singular?
#c
enum CLCommandQueueProperties {
    CLQueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
    CLQueueProfilingEnable = CL_QUEUE_PROFILING_ENABLE
};
#endc
{#enum CLCommandQueueProperties {} deriving (Show,Eq)#}

{#fun clCreateCommandQueue as createCommandQueue
  { withCLContext* `CLContext'
  , clDeviceIDPtr `CLDeviceID'
  , combineBitMasks `[CLCommandQueueProperties]'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLCommandQueue' newCLCommandQueue*
#}

newCLCommandQueue = newData CLCommandQueue clReleaseCommandQueue
foreign import ccall "&" clReleaseCommandQueue :: Releaser CLCommandQueue_

{#fun clFlush as clFlush
  { withCLCommandQueue* `CLCommandQueue'
  } -> `Int' checkSuccess-
#}

{#fun clFinish as clFinish
  { withCLCommandQueue* `CLCommandQueue'
  } -> `Int' checkSuccess-
#}

---------------
-- Properties
#c
enum CLCommandQueueInfo {
    CLQueueContext = CL_QUEUE_CONTEXT,
    CLQueueDevice = CL_QUEUE_DEVICE,
    CLQueueReferenceCount = CL_QUEUE_REFERENCE_COUNT,
    CLQueueProperties = CL_QUEUE_PROPERTIES
};
#endc
{#enum CLCommandQueueInfo {}#}
{#fun clGetCommandQueueInfo as getInfo
  { withCLCommandQueue* `CLCommandQueue'
  , cEnum `CLCommandQueueInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

-- careful of a race:
-- If the ForeignPtr is GC'd in the middle of this computation
-- and releases the CommandQueue, OpenCL could release the CLContext also.
-- So make sure the CommandQueue stays alive throughout.
clQueueContext :: CLCommandQueue -> CLContext
clQueueContext q@(CLCommandQueue fp)
    = unsafePerformIO $ withForeignPtr fp $ \_ ->
            getProp (getInfo q CLQueueContext)
                >>= retainedCLContext

clQueueDevice :: CLCommandQueue -> CLDeviceID
clQueueDevice q = CLDeviceID $ getPureProp $ getInfo q CLQueueDevice

clQueueProperties :: CLCommandQueue -> IO [CLCommandQueueProperties]
clQueueProperties queue = getFlags (getInfo queue CLQueueProperties)
                            [ CLQueueOutOfOrderExecModeEnable
                            , CLQueueProfilingEnable
                            ]

{#fun clSetCommandQueueProperty as clSetCommandQueueProperty
  { withCLCommandQueue* `CLCommandQueue'
  , combineBitMasks `[CLCommandQueueProperties]'
  , `Bool'
  , castPtr `Ptr ()' -- Ignoring the return value of old properties,
                -- since it can be extracted by clGetCommandQueueInfo.
  } -> `CLInt' checkSuccess-
#}

setCommandQueueProperty :: CLCommandQueue -> [CLCommandQueueProperties]
                                -> Bool -> IO ()
setCommandQueueProperty queue props bool
    = clSetCommandQueueProperty queue props bool nullPtr

