module System.HsOpenCL.CommandQueue(
                -- * Mid-level monadic interface
                QueueT(..),
                QueueIO,
                -- ** Running QueueT actions
                runQueueForType,
                runQueueForDevice,
                runQueueForContext,
                -- ** Monadic convenience classes
                MonadBracket(..),
                MonadQueue(..),
                module Control.Monad.Trans,
                -- ** Basic queue operations
                getContext,
                getDevice,
                -- * Commands
                Command(),
                -- waitingFor,
                waitForCommand,
                waitForCommands,
                waitForCommands_,
                -- * Command queues
                CommandQueue,
                CommandQueueProperty(..),
                createCommandQueue,
                -- * Querying properties
                queueDevice,
                queueContext,
                getQueueProperties,
                -- * Events
                Event,
                -- waitForEvents,
                -- waitForEvent,
                eventCommandQueue,
                CommandType(..),
                eventCommandType,
                getEventCommandExecutionStatus,
                ExecutionStatus(..),
                -- enqueueMarker,
                -- enqueueWaitForEvents,
                -- enqueueBarrier,
                -- ** Profiling
                -- | When 'QueueProfilingEnable' has been set for a queue,
                -- it is possible to get timing information from an 'Event' using
                -- the following functions.
                getCommandQueued,
                getCommandSubmit,
                getCommandStart,
                getCommandEnd,
                ) where

#include <OpenCL/OpenCL.h>
import System.HsOpenCL.Internal.Types
import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.Platform
import System.HsOpenCL.Platform.Foreign(CommandQueueProperty(..))

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Array.Base as Array


--------------------------------------------
-- Monadic interface
newtype QueueT m a = QueueT {runQueueT :: CommandQueue -> m a}

type QueueIO = QueueT IO

instance Functor m => Functor (QueueT m) where
    fmap f g = QueueT $ fmap f . runQueueT g

-- TODO: Applicative definition

instance Monad m => Monad (QueueT m) where
    return x = QueueT $ const $ return x
    f >>= g = QueueT $ \q -> runQueueT f q >>= flip runQueueT q . g

instance MonadTrans QueueT where
    lift = QueueT . const

instance MonadIO m => MonadIO (QueueT m) where
    liftIO = lift . liftIO

class MonadIO m => MonadBracket m where
    liftIOBracket :: (forall b . (a -> IO b) -> IO b)
                            -> (a -> m c) -> m c

instance MonadBracket IO where
    liftIOBracket f = f

instance MonadBracket m => MonadBracket (QueueT m) where
    liftIOBracket wrap f = QueueT $ \queue ->
           liftIOBracket wrap $ \x -> runQueueT (f x) queue

class MonadBracket m => MonadQueue m where
    getQueue :: m CommandQueue

instance MonadBracket m => MonadQueue (QueueT m) where
    getQueue = QueueT return

-- TODO: see if INLINEs are necessary
instance MArray a e m => MArray a e (QueueT m) where
    getBounds = lift . getBounds
    getNumElements = lift . getNumElements
    newArray bs e = lift $ Array.newArray bs e
    newArray_ = lift . newArray_
    unsafeNewArray_ = lift . unsafeNewArray_
    unsafeRead a i = lift $ unsafeRead a i
    unsafeWrite a i x = lift $ unsafeWrite a i x





--------------------------------------------

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

------------------
-- Events

{- Unused
{#fun clWaitForEvents
  { withEvents* `[Event]'&
  } -> `CLInt' checkSuccess*-
#}

waitForEvents :: MonadIO m => [Event] -> m ()
waitForEvents = liftIO . clWaitForEvents

waitForEvent :: MonadIO m => Event -> m ()
waitForEvent e = waitForEvents [e]

-- TODO: not useful yet...
waitingFor :: [Event] -> Command -> Command
waitingFor es (Command f) = Command $ \q es' e -> f q (es++es') e
-}

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
eventCommandType e = cEnum 
    (getPureProp (getEventInfo e CLEventCommandType)
        :: CInt)

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
getEventCommandExecutionStatus e = cEnum <$>
    (getProp (getEventInfo e CLEventCommandExecutionStatus)
        :: IO CInt)



-------

-- TODO: What happens if we use waitForEvents instead of finish?

-- | Enqueue and run the given 'Command's.  Blocks until all of the given
-- 'Command's have completed.
-- Returns a list of 'Event's describing the input 'Command's.
-- 
-- If 'QueueOutOfOrderExecModeEnable' has been
-- set, then the 'Command's are not guaranteed to run in the order of the list,
-- and a 'Command' may start before the previous one has finished.
-- 
waitForCommands :: MonadQueue m => [Command] -> m [Event]
waitForCommands cs = do
    q <- getQueue
    -- TODO: is the other way really faster?
    -- (es,fs) <- liftM unzip $ liftIO $ loop q [] cs
    (es,fs) <- liftM unzip $ liftIO $ forM cs $ \c -> do
                        alloca $ \p -> do
                                f <- runCommand c q [] (EventPtr p)
                                e <- peek p >>= newEvent
                                return (e,f)
    finish
    liftIO $ sequence_ fs
    return $ reverse es

newEvent :: Ptr () -> IO Event
newEvent = newData Event clReleaseEvent

foreign import ccall "&" clReleaseEvent :: Releaser Event_

-- | Behaves the same as 'waitForCommands', but does not create or return 'Event's
-- for the given 'Command's.
waitForCommands_ :: MonadQueue m => [Command] -> m ()
waitForCommands_ cs = do
    q <- getQueue
    fs <- liftIO $ loop q (return ()) cs
    finish
    liftIO fs
  where
    loop _ fs [] = return fs
    -- TODO: time with both f>>fs and f:fs, and see which is fastest.
    loop q fs (d:ds) = runCommand d q [] (EventPtr nullPtr) >>= \f -> loop q (f>>fs) ds

-- | Enqueue and run one 'Command'.  Once that 'Command' has completed, returns
-- an 'Event' describing the input 'Command'.
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

runQueueForDevice  :: MonadIO m => DeviceID -> QueueT m a -> m a
runQueueForDevice dev f = do
    cxt <- liftIO $ createContext [dev]
    runQueueForContext dev cxt f

runQueueForType :: MonadIO m => DeviceType -> QueueT m a -> m a
runQueueForType dtype f = do
    dev <- liftIO $ getDeviceID [dtype]
    runQueueForDevice dev f

runQueueForContext :: MonadIO m => DeviceID -> Context -> QueueT m a -> m a
runQueueForContext dev cxt f = do
    queue <- liftIO $ createCommandQueue cxt dev []
    runQueueT f queue

{- UNUSED

{#fun clFlush
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess*-
#}

flush :: MonadQueue m => m ()
flush = getQueue >>= liftIO . clFlush
-}

{#fun clFinish
  { withCommandQueue* `CommandQueue'
  } -> `Int' checkSuccess*-
#}

finish :: MonadQueue m => m ()
finish = getQueue >>= liftIO . clFinish
