module OpenCL.Memory(
                -- * Memory objects
                MemObject(),
                MemAccessFlag(..),
                MemInitFlag(..),
                -- ** Resource management
                releaseMemObject,
                retainMemObject,
                -- * Buffers
                Buffer,
                castBuffer,
                newBuffer,
                withBuffer,
                -- * Reading, writing and copying buffers
                IsBlocking(..),
                enqueueReadBuffer,
                enqueueWriteBuffer,
                enqueueCopyBuffer,
                -- * Properties
                memFlags,
                memSize,
                memContext,
                getMemReferenceCount,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Internal.Types
import OpenCL.Internal.C2HS
import OpenCL.Error
import Data.Maybe
import Control.Applicative
import Control.Exception
-- TODO
-- Events/blocking
--
-- Images (restric types somehow...)

castBuffer :: Buffer a -> Buffer b
castBuffer (Buffer a) = Buffer a

#c
enum CLMemFlags {
    CLMemReadWrite_ = CL_MEM_READ_WRITE,
    CLMemWriteOnly_ = CL_MEM_WRITE_ONLY,
    CLMemReadOnly_ = CL_MEM_READ_ONLY,
    CLMemUseHostPtr_ = CL_MEM_USE_HOST_PTR,
    CLMemAllocHostPtr_ = CL_MEM_ALLOC_HOST_PTR,
    CLMemCopyHostPtr_ = CL_MEM_COPY_HOST_PTR
};
#endc
{#enum CLMemFlags {} deriving (Show,Eq)#}

-- TODO: should I be using Long or size_T explicitly to prevent overflow?
{#fun clCreateBuffer
  { withContext* `Context'
  , combineBitMasks `[CLMemFlags]'
  , `Int'
  , castPtr `Ptr a'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Buffer a' newBufferObj
#}

newBufferObj :: Ptr () -> Buffer a
newBufferObj = Buffer . castPtr

data MemAccessFlag = MemReadWrite | MemWriteOnly | MemReadOnly
                    deriving (Show,Eq)

data MemInitFlag a = NoHostPtr | UseHostPtr (Ptr a)
                        | CopyHostPtr (Ptr a)
                        | AllocHostPtr
                        | CopyAllocHostPtr (Ptr a)
                    deriving (Show,Eq)

newBuffer :: forall a . Storable a
        => Context -> MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> IO (Buffer a)
newBuffer context memAccess hostPtr size
    = clCreateBuffer context flags (size * eltSize) p'
  where 
    flags = memFlag : hostPtrFlags
    eltSize = sizeOf (undefined :: a)
    (hostPtrFlags, p')
        = case hostPtr of
            NoHostPtr -> ([],nullPtr)
            UseHostPtr p -> ([CLMemUseHostPtr_],p)
            CopyHostPtr p -> ([CLMemCopyHostPtr_],p)
            AllocHostPtr -> ([CLMemAllocHostPtr_],nullPtr)
            CopyAllocHostPtr p -> ([CLMemCopyHostPtr_,CLMemAllocHostPtr_],p)
    memFlag = case memAccess of
            MemReadWrite -> CLMemReadWrite_
            MemWriteOnly -> CLMemWriteOnly_
            MemReadOnly -> CLMemReadOnly_

withBuffer :: Storable a => Context -> MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> (Buffer a -> IO b) -> IO b
withBuffer context memAccess hostPtr size f = bracket
        (newBuffer context memAccess hostPtr size)
        releaseMemObject
        f

data IsBlocking = Blocking | NonBlocking
                    deriving (Show,Eq)

blockingFlag :: IsBlocking -> CUInt
blockingFlag Blocking = 1
blockingFlag NonBlocking = 0

{#fun clEnqueueReadBuffer
  { withCommandQueue* `CommandQueue'
  , withBufferPtr* `Buffer a'
  , blockingFlag `IsBlocking'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , withEvents*`[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

enqueueReadBuffer :: forall a . Storable a 
        => CommandQueue -> Buffer a -> IsBlocking
                                -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                    -> Ptr a -> [Event] -> IO Event
enqueueReadBuffer queue mem block offset size p
    = clEnqueueReadBuffer queue mem block
                            (offset * eltWidth) (size * eltWidth) p
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueWriteBuffer
  { withCommandQueue* `CommandQueue'
  , withBufferPtr* `Buffer a'
  , blockingFlag `IsBlocking'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , withEvents*`[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

enqueueWriteBuffer :: forall a . Storable a 
        => CommandQueue -> Buffer a -> IsBlocking
                                -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                                -> Ptr a -> [Event] -> IO Event
enqueueWriteBuffer queue mem blocking offset size p
    = clEnqueueWriteBuffer queue mem blocking
                            (offset * eltWidth) (size * eltWidth) p
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueCopyBuffer
  { withCommandQueue* `CommandQueue'
  , withBufferPtr* `Buffer a'
  , withBufferPtr* `Buffer a'
  , `Int'
  , `Int'
  , `Int'
  , withEvents*`[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

enqueueCopyBuffer :: forall a . Storable a
    => CommandQueue -> Buffer a -- ^ The source buffer
            -> Buffer a -- ^ The destination buffer.
            -> Int -- ^ The offset index in the source buffer.
            -> Int -- ^ The offset index in the destination.
            -> Int -- ^ The number of elements to copy.
            -> [Event] -> IO Event
enqueueCopyBuffer queue source dest srcOff destOff size
    = clEnqueueCopyBuffer queue source dest
            (srcOff * eltWidth)
            (destOff * eltWidth)
            (size * eltWidth)
  where eltWidth = sizeOf (undefined :: a)


{#fun clRetainMemObject as retainMemObject
  `MemObject m' =>
  { withMemObject* `m'
  } -> `Int' checkSuccess-
#}

{#fun clReleaseMemObject as releaseMemObject
  `MemObject m' =>
  { withMemObject* `m'
  } -> `Int' checkSuccess-
#}

{#fun clGetMemObjectInfo as getMemInfo
  `MemObject m' =>
  { withMemObject* `m'
  , cEnum `CLMemInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess *-
#}

#c
enum CLMemInfo {
    CLMemType = CL_MEM_TYPE,
    CLMemFlags = CL_MEM_FLAGS,
    CLMemSize = CL_MEM_SIZE,
    CLMemHostPtr = CL_MEM_HOST_PTR,
    CLMemMapCount = CL_MEM_MAP_COUNT,
    CLMemReferenceCount = CL_MEM_REFERENCE_COUNT,
    CLMemContext = CL_MEM_CONTEXT
};
#endc
{#enum CLMemInfo {} #}


memFlags :: MemObject m => m -> (MemAccessFlag, MemInitFlag a)
memFlags m = (accessFlag exists, memInit)
  where
    exists = getPureProp (getMemInfo m CLMemFlags)
    memInit = getMemInitFlags exists
    accessFlag exists
        | exists CLMemReadOnly_ = MemReadOnly
        | exists CLMemWriteOnly_ = MemWriteOnly
        | otherwise = MemReadWrite
    getPtrProp constr = constr $ getPureProp (getMemInfo m CLMemHostPtr)
    getMemInitFlags exists
        | exists CLMemUseHostPtr_   = getPtrProp UseHostPtr
        | exists CLMemCopyHostPtr_
            = if exists CLMemAllocHostPtr_ then getPtrProp CopyAllocHostPtr
                    else getPtrProp CopyHostPtr
        | exists CLMemAllocHostPtr_ = AllocHostPtr
        | otherwise = NoHostPtr
        
-- | Size of the data store, in bytes.
memSize :: MemObject m => m -> Int
memSize m = getPureProp (getMemInfo m CLMemSize)

-- Since the Mem doesn't have an associated finalizer, we don't have to
-- worry about races like we do in clQueue.
-- Well, there's still a tiny race if another thread releases the Mem and 
-- context in the middle of our computation; but it's reasonable
-- to expect that the Buffer (and thus, OpenCL ensures, the Context) isn't
-- freed by other threads during this computation.
memContext :: MemObject m => m -> Context
memContext m = unsafePerformIO $
                    getProp (getMemInfo m CLMemContext) >>= newContext

getMemReferenceCount :: MemObject m => m -> IO Int
getMemReferenceCount m
    = getProp (getMemInfo m CLMemReferenceCount)

--------
--  Opaque class for cl_mem
-- 
-- There's a lot of ways we could go with the mem objects;
-- but this API shouldn't add too much to what's already in OpenCL.

class MemObject m where
    withMemObject :: m -> (Ptr () -> IO a) -> IO a

instance MemObject (Buffer a) where
    withMemObject = withBufferPtr
