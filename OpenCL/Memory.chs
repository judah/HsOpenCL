module OpenCL.Memory(
                -- * Memory objects
                CLMem,
                castCLMem,
                createBuffer,
                CLMemAccess(..),
                CLMemInit(..),
                clRetainMemObject,
                clReleaseMemObject,
                -- * Reading, writing and copying buffers
                enqueueReadBuffer,
                enqueueReadBufferOff,
                enqueueWriteBuffer,
                enqueueWriteBufferOff,
                enqueueCopyBuffer,
                enqueueCopyBufferOff,
                -- * Properties
                clMemFlags,
                clMemSize,
                clMemContext,
                clGetMemReferenceCount,
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

castCLMem :: CLMem a -> CLMem b
castCLMem (CLMem a) = CLMem a

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
  } -> `CLMem a' newCLMem
#}

newCLMem :: Ptr () -> CLMem a
newCLMem = CLMem . castPtr

data CLMemAccess = CLMemReadWrite | CLMemWriteOnly | CLMemReadOnly
                    deriving (Show,Eq)

data CLMemInit a = NoHostPtr | UseHostPtr (Ptr a)
                        | CopyHostPtr (Ptr a)
                        | AllocHostPtr
                        | CopyAllocHostPtr (Ptr a)
                    deriving (Show,Eq)

createBuffer :: forall a . Storable a
        => Context -> CLMemAccess -> CLMemInit a -> Int -> IO (CLMem a)
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
            CLMemReadWrite -> CLMemReadWrite_
            CLMemWriteOnly -> CLMemWriteOnly_
            CLMemReadOnly -> CLMemReadOnly_

{#fun clEnqueueReadBuffer
  { withCommandQueue* `CommandQueue'
  , withCLMem* `CLMem a'
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
        => CommandQueue -> CLMem a
                -> Int -- ^ The number of elements to copy.
                -> Ptr a -> IO ()
enqueueReadBuffer queue mem = enqueueReadBufferOff queue mem 0

enqueueReadBufferOff :: forall a . Storable a
        => CommandQueue -> CLMem a -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                    -> Ptr a -> IO ()
enqueueReadBufferOff queue mem offset size p
    = clEnqueueReadBuffer queue mem True (offset * eltWidth) (size * eltWidth)
                    p 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueWriteBuffer
  { withCommandQueue* `CommandQueue'
  , withCLMem* `CLMem a'
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
        => CommandQueue -> CLMem a
                -> Int  -- ^ The number of elements to copy.
                -> Ptr a -> IO ()
enqueueWriteBuffer queue mem = enqueueWriteBufferOff queue mem 0

enqueueWriteBufferOff :: forall a . Storable a
        => CommandQueue -> CLMem a -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                                -> Ptr a -> IO ()
enqueueWriteBufferOff queue mem offset size p
    = clEnqueueWriteBuffer queue mem True (offset * eltWidth) (size * eltWidth)
                    p 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueCopyBuffer
  { withCommandQueue* `CommandQueue'
  , withCLMem* `CLMem a'
  , withCLMem* `CLMem a'
  , `Int'
  , `Int'
  , `Int'
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `Int' checkSuccess-
#}

enqueueCopyBuffer :: forall a . Storable a
    => CommandQueue -> CLMem a -- ^ The source buffer.
            -> CLMem a -- ^ The destination buffer.
            -> Int -- ^ The number of elements to copy.
            -> IO ()
enqueueCopyBuffer queue source dest
    = enqueueCopyBufferOff queue source dest 0 0

enqueueCopyBufferOff :: forall a . Storable a
    => CommandQueue -> CLMem a -- ^ The source buffer
            -> CLMem a -- ^ The destination buffer.
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


{#fun clRetainMemObject
  { withCLMem* `CLMem a'
  } -> `Int' checkSuccess-
#}

{#fun clReleaseMemObject
  { withCLMem* `CLMem a'
  } -> `Int' checkSuccess-
#}


{#fun clGetMemObjectInfo as getMemInfo
  { withCLMem* `CLMem a'
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


clMemFlags :: CLMem a -> (CLMemAccess, CLMemInit a)
clMemFlags m = (accessFlag exists, memInit)
  where
    exists = getPureProp (getMemInfo m CLMemFlags)
    memInit = getMemInitFlags exists
    accessFlag exists
        | exists CLMemReadOnly_ = CLMemReadOnly
        | exists CLMemWriteOnly_ = CLMemWriteOnly
        | otherwise = CLMemReadWrite
    getPtrProp constr = constr $ getPureProp (getMemInfo m CLMemHostPtr)
    getMemInitFlags exists
        | exists CLMemUseHostPtr_   = getPtrProp UseHostPtr
        | exists CLMemCopyHostPtr_
            = if exists CLMemAllocHostPtr_ then getPtrProp CopyAllocHostPtr
                    else getPtrProp CopyHostPtr
        | exists CLMemAllocHostPtr_ = AllocHostPtr
        | otherwise = NoHostPtr
        

clMemSize :: forall a . Storable a => CLMem a -> Int
clMemSize m = getPureProp (getMemInfo m CLMemSize)
                `div` sizeOf (undefined :: a)

-- Since the Mem doesn't have an associated finalizer, we don't have to
-- worry about races like we do in clQueue.
-- Well, there's still a tiny race if another thread releases the Mem and 
-- context in the middle of our computation; but it's reasonable
-- to expect that the CLMem (and thus, OpenCL ensures, the Context) isn't
-- freed by other threads during this computation.
clMemContext :: CLMem a -> Context
clMemContext m = unsafePerformIO $
                    getProp (getMemInfo m CLMemContext) >>= newContext

clGetMemReferenceCount :: CLMem a -> IO Int
clGetMemReferenceCount m
    = getProp (getMemInfo m CLMemReferenceCount)
