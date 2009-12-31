module OpenCL.CommandQueue(
                -- * Command queues
                CommandQueue,
                CommandQueueProperty(..),
                createCommandQueue,
                flush,
                finish,
                -- ** Commands
                Command,
                enqueue,
                enqueue_,
                enqueues_,
                waitForCommand,
                waitForCommands,
                -- * Querying info and properties
                queueDevice,
                queueContext,
                getQueueProperties,
                setQueueProperties,
                -- * Events
                Event,
                waitForEvents,
                waitForEvent,
                eventCommandQueue,
                CommandType(..),
                eventCommandType,
                getEventCommandExecutionStatus,
                ExecutionStatus(..),
                enqueueMarker,
                enqueueWaitForEvents,
                enqueueBarrier,
                -- ** Profiling
                getCommandQueued,
                getCommandSubmit,
                getCommandStart,
                getCommandEnd,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Internal.Types
import OpenCL.Internal.C2HS
import OpenCL.Error

import Control.Applicative

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

newCommandQueue :: Ptr () -> IO CommandQueue
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

------------------
-- Events

{#fun clWaitForEvents as waitForEvents
  { withEvents* `[Event]'&
  } -> `CLInt' checkSuccess-
#}

waitForEvent :: Event -> IO ()
waitForEvent e = waitForEvents [e]

{#fun clGetEventInfo as getEventInfo
  { withEvent* `Event'
  , cEnum `CLEventInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

#c
enum CLEventInfo {
    CLEventCommandQueue = CL_EVENT_COMMAND_QUEUE,
    CLEventCommandType = CL_EVENT_COMMAND_TYPE,
    CLEventReferenceCount = CL_EVENT_REFERENCE_COUNT,
    CLEventCommandExecutionStatus = CL_EVENT_COMMAND_EXECUTION_STATUS,
};
#endc
{#enum CLEventInfo {} #}
    
eventCommandQueue :: Event -> CommandQueue
eventCommandQueue e@(Event fp)
    = unsafePerformIO $ withForeignPtr fp $ \_ -> do
        p <- getProp (getEventInfo e CLEventCommandQueue)
        clRetainCommandQueue p
        newCommandQueue p

{#fun clRetainCommandQueue
  { id `Ptr ()'
  } -> `Int' checkSuccess*-
#}


#c
enum CommandType {
    CommandNdrangeKernel = CL_COMMAND_NDRANGE_KERNEL,
    CommandTask = CL_COMMAND_TASK,
    CommandNativeKernel = CL_COMMAND_NATIVE_KERNEL,
    CommandReadBuffer = CL_COMMAND_READ_BUFFER,
    CommandWriteBuffer = CL_COMMAND_WRITE_BUFFER,
    CommandCopyBuffer = CL_COMMAND_COPY_BUFFER,
    CommandReadImage = CL_COMMAND_READ_IMAGE,
    CommandWriteImage = CL_COMMAND_WRITE_IMAGE,
    CommandCopyImage = CL_COMMAND_COPY_IMAGE,
    CommandCopyImageToBuffer = CL_COMMAND_COPY_IMAGE_TO_BUFFER,
    CommandCopyBufferToImage = CL_COMMAND_COPY_BUFFER_TO_IMAGE,
    CommandMapBuffer = CL_COMMAND_MAP_BUFFER,
    CommandMapImage = CL_COMMAND_MAP_IMAGE,
    CommandUnmapMemObject = CL_COMMAND_UNMAP_MEM_OBJECT,
    CommandMarker = CL_COMMAND_MARKER,
    CommandAcquireGlObjects = CL_COMMAND_ACQUIRE_GL_OBJECTS,
    CommandReleaseGlObjects = CL_COMMAND_RELEASE_GL_OBJECTS
};
#endc
{#enum CommandType {} deriving (Show,Eq)#}

eventCommandType :: Event -> CommandType
eventCommandType e = toEnum $ getPureProp (getEventInfo e CLEventCommandType)

#c
enum ExecutionStatus {
    Complete = CL_COMPLETE,
    Running = CL_RUNNING,
    Submitted = CL_SUBMITTED,
    Queued = CL_QUEUED
};
#endc
{#enum ExecutionStatus {} deriving (Show,Eq)#}

getEventCommandExecutionStatus :: Event -> IO ExecutionStatus
getEventCommandExecutionStatus e = toEnum <$>
    getProp (getEventInfo e CLEventCommandExecutionStatus)



-------
enqueue :: CommandQueue -> Command -> [Event] -> IO Event
enqueue q (Command f) es = withEvents es $ \(n,es_p) ->
                            alloca $ \e_p -> do
                                f q n es_p e_p
                                newEvent e_p

enqueue_ :: CommandQueue -> Command -> IO ()
enqueue_ q (Command f) = f q 0 nullPtr nullPtr

enqueues_ :: CommandQueue -> [Command] -> IO ()
enqueues_ q = mapM_ (enqueue_ q)

waitForCommand :: CommandQueue -> Command -> IO ()
waitForCommand q c = enqueue q c [] >>= waitForEvent

waitForCommands :: CommandQueue -> [Command] -> IO ()
waitForCommands q cs = mapM (\c -> enqueue q c []) cs >>= waitForEvents

--------------------------

-- how would this fit in?
-- return a "wait for event" marker?
{#fun clEnqueueMarker as enqueueMarker
  { withCommandQueue* `CommandQueue'
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

{#fun clEnqueueWaitForEvents as enqueueWaitForEvents
  { withCommandQueue* `CommandQueue'
  , withEvents* `[Event]'&
  } -> `Int' checkSuccess-
#}

{#fun clEnqueueBarrier as enqueueBarrier
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess-
#}

-----------------
-- Profiling
#c
enum CLProfilingInfo {
    CLProfilingCommandQueued = CL_PROFILING_COMMAND_QUEUED,
    CLProfilingCommandSubmit = CL_PROFILING_COMMAND_SUBMIT,
    CLProfilingCommandStart = CL_PROFILING_COMMAND_START,
    CLProfilingCommandEnd = CL_PROFILING_COMMAND_END,
};
#endc
{#enum CLProfilingInfo {} #}


{#fun clGetEventProfilingInfo as getProfInfo
  { withEvent* `Event'
  , cEnum `CLProfilingInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

getCommandQueued :: Event -> IO Word64
getCommandQueued e = getProp (getProfInfo e CLProfilingCommandQueued)

getCommandSubmit :: Event -> IO Word64
getCommandSubmit e = getProp (getProfInfo e CLProfilingCommandSubmit)

getCommandStart :: Event -> IO Word64
getCommandStart e = getProp (getProfInfo e CLProfilingCommandStart)

getCommandEnd :: Event -> IO Word64
getCommandEnd e = getProp (getProfInfo e CLProfilingCommandEnd)

