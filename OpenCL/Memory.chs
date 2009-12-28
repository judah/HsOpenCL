module OpenCL.Memory(
                -- * Memory objects
                MemObject(),
                Buffer,
                castBuffer,
                createBuffer,
                MemAccessFlag(..),
                MemInitFlag(..),
                retainMemObject,
                releaseMemObject,
                -- * Reading, writing and copying buffers
                enqueueReadBuffer,
                enqueueReadBufferOff,
                enqueueWriteBuffer,
                enqueueWriteBufferOff,
                enqueueCopyBuffer,
                enqueueCopyBufferOff,
                -- * Properties
                memFlags,
                memSize,
                memContext,
                getMemReferenceCount,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error
import Data.Maybe
import Control.Applicative

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
  } -> `Buffer a' newBuffer
#}

newBuffer :: Ptr () -> Buffer a
newBuffer = Buffer . castPtr

data MemAccessFlag = MemReadWrite | MemWriteOnly | MemReadOnly
                    deriving (Show,Eq)

data MemInitFlag a = NoHostPtr | UseHostPtr (Ptr a)
                        | CopyHostPtr (Ptr a)
                        | AllocHostPtr
                        | CopyAllocHostPtr (Ptr a)
                    deriving (Show,Eq)

createBuffer :: forall a . Storable a
        => Context -> MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> IO (Buffer a)
createBuffer context memAccess hostPtr size
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

{#fun clEnqueueReadBuffer
  { withCommandQueue* `CommandQueue'
  , withBuffer* `Buffer a'
  , cFromBool `Bool'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `Int' checkSuccess-
#}

enqueueReadBuffer :: Storable a 
        => CommandQueue -> Buffer a
                -> Int -- ^ The number of elements to copy.
                -> Ptr a -> IO ()
enqueueReadBuffer queue mem = enqueueReadBufferOff queue mem 0

enqueueReadBufferOff :: forall a . Storable a
        => CommandQueue -> Buffer a -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                    -> Ptr a -> IO ()
enqueueReadBufferOff queue mem offset size p
    = clEnqueueReadBuffer queue mem True (offset * eltWidth) (size * eltWidth)
                    p 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueWriteBuffer
  { withCommandQueue* `CommandQueue'
  , withBuffer* `Buffer a'
  , cFromBool `Bool'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `Int' checkSuccess-
#}

enqueueWriteBuffer :: Storable a 
        => CommandQueue -> Buffer a
                -> Int  -- ^ The number of elements to copy.
                -> Ptr a -> IO ()
enqueueWriteBuffer queue mem = enqueueWriteBufferOff queue mem 0

enqueueWriteBufferOff :: forall a . Storable a
        => CommandQueue -> Buffer a -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                                -> Ptr a -> IO ()
enqueueWriteBufferOff queue mem offset size p
    = clEnqueueWriteBuffer queue mem True (offset * eltWidth) (size * eltWidth)
                    p 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueCopyBuffer
  { withCommandQueue* `CommandQueue'
  , withBuffer* `Buffer a'
  , withBuffer* `Buffer a'
  , `Int'
  , `Int'
  , `Int'
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `Int' checkSuccess-
#}

enqueueCopyBuffer :: forall a . Storable a
    => CommandQueue -> Buffer a -- ^ The source buffer.
            -> Buffer a -- ^ The destination buffer.
            -> Int -- ^ The number of elements to copy.
            -> IO ()
enqueueCopyBuffer queue source dest
    = enqueueCopyBufferOff queue source dest 0 0

enqueueCopyBufferOff :: forall a . Storable a
    => CommandQueue -> Buffer a -- ^ The source buffer
            -> Buffer a -- ^ The destination buffer.
            -> Int -- ^ The offset index in the source buffer.
            -> Int -- ^ The offset index in the destination.
            -> Int -- ^ The number of elements to copy.
            -> IO ()
enqueueCopyBufferOff queue source dest srcOff destOff size
    = clEnqueueCopyBuffer queue source dest
            (srcOff * eltWidth)
            (destOff * eltWidth)
            (size * eltWidth) 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)


{#fun clRetainMemObject as retainMemObject
  `MemObject m' =>
  { memObjectPtr `m'
  } -> `Int' checkSuccess-
#}

{#fun clReleaseMemObject as releaseMemObject
  `MemObject m' =>
  { memObjectPtr `m'
  } -> `Int' checkSuccess-
#}


{#fun clGetMemObjectInfo as getMemInfo
  `MemObject m' =>
  { memObjectPtr `m'
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
    memObjectPtr :: m -> Ptr ()

instance MemObject (Buffer a) where
    memObjectPtr (Buffer p) = castPtr p
