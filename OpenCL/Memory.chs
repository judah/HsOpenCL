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
                    deriving (Show,Eq)

data CLMemInit a = NoHostPtr | UseHostPtr (Ptr a)
                        | CopyHostPtr (Ptr a)
                        | AllocHostPtr
                        | CopyAllocHostPtr (Ptr a)
                    deriving (Show,Eq)

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


{#fun clGetMemObjectInfo
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

storableInfo :: forall a b . Storable a => CLMemInfo -> CLMem b -> IO a
storableInfo info mem = alloca $ \p -> do
    retSize <- clGetMemObjectInfo mem info (sizeOf (undefined :: a)) (castPtr p)
    peek p


clMemFlags :: CLMem a -> (CLMemAccess, CLMemInit a)
clMemFlags m = unsafePerformIO $ do
    bitmask :: CULLong <- storableInfo CLMemFlags m
    let exists = containsBitMask bitmask
    memInit <- getMemInitFlags exists
    return (accessFlag exists,memInit)
  where
    accessFlag exists
        | exists CLMemReadOnly_ = CLMemReadOnly
        | exists CLMemWriteOnly_ = CLMemWriteOnly
        | otherwise = CLMemReadWrite
    getPtrProp constr = constr <$> storableInfo CLMemHostPtr m
    getMemInitFlags exists
        | exists CLMemUseHostPtr_   = getPtrProp UseHostPtr
        | exists CLMemCopyHostPtr_
            = if exists CLMemAllocHostPtr_ then getPtrProp CopyAllocHostPtr
                    else getPtrProp CopyHostPtr
        | exists CLMemAllocHostPtr_ = return AllocHostPtr
        | otherwise = return NoHostPtr
        

clMemSize :: forall a . Storable a => CLMem a -> Int
clMemSize m = fromEnum (unsafePerformIO $ storableInfo CLMemSize m :: CInt)
                `div` sizeOf (undefined :: a)

-- Since the Mem doesn't have an associated finalizer, we don't have to
-- worry about races like we do in clQueue.
-- Well, there's still a tiny race if another thread releases the Mem and 
-- context in the middle of our computation; but it's reasonable
-- to expect that the CLMem (and thus, OpenCL ensures, the CLContext) isn't
-- freed by other threads during this computation.
clMemContext :: CLMem a -> CLContext
clMemContext m = unsafePerformIO $ storableInfo CLMemContext m >>= newCLContext

clGetMemReferenceCount :: CLMem a -> IO Int
clGetMemReferenceCount m = fromEnum <$>
        (storableInfo CLMemReferenceCount m :: IO CInt)
