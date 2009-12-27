module OpenCL.CommandQueue(
                -- * Command queues
                CommandQueue,
                CommandQueueProperty(..),
                createCommandQueue,
                flush,
                finish,
                -- * Querying info and properties
                queueDevice,
                queueContext,
                getQueueProperties,
                setQueueProperties,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error

import Control.Monad

-- TODO: should this be called "Property" in singular?
#c
enum CommandQueueProperty {
    QueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
    QueueProfilingEnable = CL_QUEUE_PROFILING_ENABLE
};
#endc
{#enum CommandQueueProperty {} deriving (Show,Eq)#}

{#fun clCreateCommandQueue as createCommandQueue
  { withContext* `Context'
  , deviceIDPtr `DeviceID'
  , combineBitMasks `[CommandQueueProperty]'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CommandQueue' newCommandQueue*
#}

newCommandQueue = newData CommandQueue clReleaseCommandQueue
foreign import ccall "&" clReleaseCommandQueue :: Releaser CommandQueue_

{#fun clFlush as flush
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess-
#}

{#fun clFinish as finish
  { withCommandQueue* `CommandQueue'
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
  { withCommandQueue* `CommandQueue'
  , cEnum `CLCommandQueueInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

-- careful of a race:
-- If the ForeignPtr is GC'd in the middle of this computation
-- and releases the CommandQueue, OpenCL could release the Context also.
-- So make sure the CommandQueue stays alive throughout.
queueContext :: CommandQueue -> Context
queueContext q@(CommandQueue fp)
    = unsafePerformIO $ withForeignPtr fp $ \_ ->
            getProp (getInfo q CLQueueContext)
                >>= retainedCLContext

queueDevice :: CommandQueue -> DeviceID
queueDevice q = DeviceID $ getPureProp $ getInfo q CLQueueDevice

getQueueProperties :: CommandQueue -> IO [CommandQueueProperty]
getQueueProperties queue = getFlags (getInfo queue CLQueueProperties)
                            [ QueueOutOfOrderExecModeEnable
                            , QueueProfilingEnable
                            ]

{#fun clSetCommandQueueProperty as clSetCommandQueueProperty
  { withCommandQueue* `CommandQueue'
  , combineBitMasks `[CommandQueueProperty]'
  , `Bool'
  , castPtr `Ptr ()' -- Ignoring the return value of old properties,
                -- since it can be extracted by clGetCommandQueueInfo.
  } -> `CLInt' checkSuccess-
#}

setQueueProperties :: CommandQueue -> [CommandQueueProperty]
                                -> Bool -> IO ()
setQueueProperties queue props bool
    = clSetCommandQueueProperty queue props bool nullPtr

