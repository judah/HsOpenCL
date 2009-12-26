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
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error
import Data.Maybe

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
  { withCLContext* `CLContext'
  , combineBitMasks `[CLMemFlags]'
  , `Int'
  , castPtr `Ptr a'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLMem a' newCLMem
#}

newCLMem :: Ptr () -> CLMem a
newCLMem = CLMem . castPtr

data CLMemAccess = CLMemReadWrite | CLMemWriteOnly | CLMemReadOnly

data CLMemInit a = NoHostPtr | UseHostPtr (Ptr a)
                        | CopyHostPtr (Ptr a)
                        | AllocHostPtr
                        | CopyAllocHostPtr (Ptr a)

createBuffer :: forall a . Storable a
        => CLContext -> CLMemAccess -> CLMemInit a -> Int -> IO (CLMem a)
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
  { withCLCommandQueue* `CLCommandQueue'
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
        => CLCommandQueue -> CLMem a
                -> Int -- ^ The number of elements to copy.
                -> Ptr a -> IO ()
enqueueReadBuffer queue mem = enqueueReadBufferOff queue mem 0

enqueueReadBufferOff :: forall a . Storable a
        => CLCommandQueue -> CLMem a -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                    -> Ptr a -> IO ()
enqueueReadBufferOff queue mem offset size p
    = clEnqueueReadBuffer queue mem True (offset * eltWidth) (size * eltWidth)
                    p 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueWriteBuffer
  { withCLCommandQueue* `CLCommandQueue'
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
        => CLCommandQueue -> CLMem a
                -> Int  -- ^ The number of elements to copy.
                -> Ptr a -> IO ()
enqueueWriteBuffer queue mem = enqueueWriteBufferOff queue mem 0

enqueueWriteBufferOff :: forall a . Storable a
        => CLCommandQueue -> CLMem a -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                                -> Ptr a -> IO ()
enqueueWriteBufferOff queue mem offset size p
    = clEnqueueWriteBuffer queue mem True (offset * eltWidth) (size * eltWidth)
                    p 0 nullPtr nullPtr
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueCopyBuffer
  { withCLCommandQueue* `CLCommandQueue'
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
    => CLCommandQueue -> CLMem a -- ^ The source buffer.
            -> CLMem a -- ^ The destination buffer.
            -> Int -- ^ The number of elements to copy.
            -> IO ()
enqueueCopyBuffer queue source dest
    = enqueueCopyBufferOff queue source dest 0 0

enqueueCopyBufferOff :: forall a . Storable a
    => CLCommandQueue -> CLMem a -- ^ The source buffer
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
