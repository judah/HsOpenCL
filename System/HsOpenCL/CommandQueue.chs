module System.HsOpenCL.CommandQueue(
                -- * Command queues
                CommandQueue,
                CommandQueueProperty(..),
                createCommandQueue,
                -- * The Queue monad
                runQueueForType,
                runQueueForDevice,
                runQueueForContext,
                getContext,
                getDevice,
                setProperties,
                -- flush,
                -- finish,
                -- ** Commands
                Command(..),
                -- waitingFor,
                waitForCommand,
                waitForCommands,
                waitForCommands_,
                mkCommand,
                -- * Querying info and properties
                queueDevice,
                queueContext,
                getQueueProperties,
                setQueueProperties,
                -- * Events
                Event,
                -- waitForEvents,
                -- waitForEvent,
                -- ** Lower-level
                eventCommandQueue,
                CommandType(..),
                eventCommandType,
                getEventCommandExecutionStatus,
                ExecutionStatus(..),
                -- enqueueMarker,
                -- enqueueWaitForEvents,
                -- enqueueBarrier,
                -- ** Profiling
                getCommandQueued,
                getCommandSubmit,
                getCommandStart,
                getCommandEnd,
                ) where

#include <OpenCL/OpenCL.h>
import System.HsOpenCL.Internal.Types
import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.MonadQueue
import System.HsOpenCL.Platform
import System.HsOpenCL.Platform.Foreign(CommandQueueProperty(..))

import Control.Applicative
import Control.Monad
import Control.Monad.Trans


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
    = unsafePerformIO $ withForeignPtr fp $ \_ -> do
        p <- getProp (getInfo q CLQueueContext)
        c <- retainedCLContext p
        -- contextRefCount c >>= \n -> print ("Context-ref:",p,n)
        return c

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
  } -> `CLInt' checkSuccess*-
#}

setQueueProperties :: CommandQueue -> [CommandQueueProperty]
                                -> Bool -> IO ()
setQueueProperties queue props bool
    = clSetCommandQueueProperty queue props bool nullPtr

------------------
-- Events

{#fun clWaitForEvents
  { withEvents* `[Event]'&
  } -> `CLInt' checkSuccess*-
#}

waitForEvents :: MonadIO m => [Event] -> m ()
waitForEvents = liftIO . clWaitForEvents

waitForEvent :: MonadIO m => Event -> m ()
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
-- TODO: just Command {finalize :: IO, runCommand :: ...->IO Event}???
newtype Command = Command {runCommand :: CommandQueue ->
                            [Event] -> Ptr (Ptr ()) -> IO (IO ())}

-- TODO: data EventPtr to wrap the Ptr (Ptr ())?

mkCommand :: (CommandQueue -> [Event] -> Ptr (Ptr ()) -> IO ())
                    -> Command
mkCommand f = Command $ \q es e -> f q es e >> return (return ())

waitingFor :: [Event] -> Command -> Command
waitingFor es (Command f) = Command $ \q es' e -> f q (es++es') e

-- TODO: What happens if we use waitForEvents instead of finish?
waitForCommands :: MonadQueue m => [Command] -> m [Event]
waitForCommands cs = do
    q <- getQueue
    -- TODO: is the other way really faster?
    -- (es,fs) <- liftM unzip $ liftIO $ loop q [] cs
    (es,fs) <- liftM unzip $ liftIO $ forM cs $ \c -> do
                        alloca $ \p -> do
                                f <- runCommand c q [] p
                                e <- peek p >>= newEvent
                                return (e,f)
    finish
    liftIO $ sequence_ fs
    return $ reverse es
  where
    loop q es [] = return es
    loop q es (c:cs) = do
        ef <- alloca $ \p -> do
                f <- runCommand c q [] p
                e <- peek p >>= newEvent
                return (e,f)
        loop q (ef:es) cs

newEvent :: Ptr () -> IO Event
newEvent = newData Event clReleaseEvent

foreign import ccall "&" clReleaseEvent :: Releaser Event_


waitForCommands_ :: MonadQueue m => [Command] -> m ()
waitForCommands_ cs = do
    q <- getQueue
    fs <- liftIO $ loop q (return ()) cs
    finish
    liftIO fs
  where
    loop q fs [] = return fs
    -- TODO: time with both f>>fs and f:fs, and see which is fastest.
    loop q fs (c:cs) = runCommand c q [] nullPtr >>= \f -> loop q (f>>fs) cs


waitForCommand :: MonadQueue m => Command -> m Event
waitForCommand c = do
    [e] <- waitForCommands [c]
    return e

--------------------------
{-

-- how would this fit in?
-- return a "wait for event" marker?
{#fun clEnqueueMarker as enqueueMarker
  { withCommandQueue* `CommandQueue'
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess*-
#}

{#fun clEnqueueWaitForEvents as enqueueWaitForEvents
  { withCommandQueue* `CommandQueue'
  , withEvents* `[Event]'&
  } -> `Int' checkSuccess*-
#}

{#fun clEnqueueBarrier as enqueueBarrier
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess*-
#}

-}

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

getContext :: MonadQueue m => m Context
getContext = liftM queueContext getQueue

getDevice :: MonadQueue m => m DeviceID
getDevice = liftM queueDevice getQueue

setProperties :: MonadQueue m => [CommandQueueProperty] -> Bool -> m ()
setProperties props bool = do
    queue <- getQueue
    liftIO $ setQueueProperties queue props bool

runQueueForDevice  :: MonadIO m => DeviceID -> QueueT m a -> m a
runQueueForDevice dev f = do
    cxt <- liftIO $ createContext [dev]
    runQueueForContext dev cxt f

runQueueForType :: MonadIO m => DeviceType -> QueueT m a -> m a
runQueueForType dtype f = do
    dev <- liftIO $ getDeviceID dtype
    runQueueForDevice dev f

runQueueForContext :: MonadIO m => DeviceID -> Context -> QueueT m a -> m a
runQueueForContext dev cxt f = do
    queue <- liftIO $ createCommandQueue cxt dev []
    runQueueT f queue

{#fun clFlush
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess*-
#}

flush :: MonadQueue m => m ()
flush = getQueue >>= liftIO . clFlush

{#fun clFinish
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess*-
#}

finish :: MonadQueue m => m ()
finish = getQueue >>= liftIO . clFinish
