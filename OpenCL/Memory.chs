module OpenCL.Memory(
                -- * Memory objects
                MemObject(),
                MemAccessFlag(..),
                MemInitFlag(..),
                -- * Buffers
                Buffer,
                castBuffer,
                newBuffer,
                allocaBuffer,
                touchBuffer,
                -- * Reading, writing and copying buffers
                IsBlocking(..),
                readBuffer,
                writeBuffer,
                copyBuffer,
                -- ** Convenience class
                CopyTo(..),
                BufferLike(..),
                Slice(),
                slice,
                sizeS,
                -- * Properties
                memFlags,
                memSize,
                bufferSize,
                memContext,
                getMemReferenceCount,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Internal.Types
import OpenCL.Internal.C2HS
import OpenCL.CommandQueue(Command(..))
import OpenCL.Error
import OpenCL.MonadQueue
import OpenCL.CommandQueue
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
  } -> `Ptr Buffer_' castPtr
#}

data MemAccessFlag = MemReadWrite | MemWriteOnly | MemReadOnly
                    deriving (Show,Eq)

data MemInitFlag a = NoHostPtr | UseHostPtr (Ptr a)
                        | CopyHostPtr (Ptr a)
                        | AllocHostPtr
                        | CopyAllocHostPtr (Ptr a)
                    deriving (Show,Eq)

newBuffer :: forall a m . (Storable a, MonadQueue m)
        => MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> m (Buffer a)
newBuffer memAccess memInit size = do
    cxt <- getContext
    p <- liftIO $ newBuffer' cxt memAccess memInit size
    liftIO $ Buffer <$> newForeignPtr clReleaseMemObject p

newBuffer' :: forall a . (Storable a)
        => Context -> MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> IO (Ptr Buffer_)
newBuffer' context memAccess hostPtr size
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

touchBuffer :: MonadIO m => Buffer a -> m ()
touchBuffer (Buffer f) = liftIO $ touchForeignPtr f

-- | Note the buffer is freed at the end of this block, so it is unsafe to use it
-- outside of the block.  (Unless you use 'touchMemObject'.)
allocaBuffer :: (Storable a, MonadQueue m) => MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> (Buffer a -> m b) -> m b
allocaBuffer memAccess hostPtr size f = do
    context <- getContext
    liftIOBracket (bracket
        (newBuffer' context memAccess hostPtr size)
        (releaseMemObject . castPtr))
      $ \p -> liftIO (newForeignPtr_ p) >>= f . Buffer

data IsBlocking = Blocking | NonBlocking
                    deriving (Show,Eq)

blockingFlag :: IsBlocking -> CUInt
blockingFlag Blocking = 1
blockingFlag NonBlocking = 0

{#fun clEnqueueReadBuffer
  { withCommandQueue* `CommandQueue'
  , withBuffer* `Buffer a'
  , blockingFlag `IsBlocking'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , withEvents* `[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

readBuffer :: forall a . Storable a
        => Buffer a -> IsBlocking
                                -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                    -> Ptr a -> Command
readBuffer mem block offset size p
    = Command $ \queue -> clEnqueueReadBuffer queue mem block
                            (offset * eltWidth) (size * eltWidth) p
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueWriteBuffer
  { withCommandQueue* `CommandQueue'
  , withBuffer* `Buffer a'
  , blockingFlag `IsBlocking'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , withEvents* `[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

writeBuffer :: forall a . Storable a => Buffer a -> IsBlocking
                                -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                                -> Ptr a -> Command
writeBuffer mem blocking offset size p
    = Command $ \queue -> clEnqueueWriteBuffer queue mem blocking
                            (offset * eltWidth) (size * eltWidth) p
  where eltWidth = sizeOf (undefined :: a)

{#fun clEnqueueCopyBuffer
  { withCommandQueue* `CommandQueue'
  , withBuffer* `Buffer a'
  , withBuffer* `Buffer a'
  , `Int'
  , `Int'
  , `Int'
  , withEvents* `[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

copyBuffer :: forall a . Storable a
    => Buffer a -- ^ The source buffer
            -> Buffer a -- ^ The destination buffer.
            -> Int -- ^ The offset index in the source buffer.
            -> Int -- ^ The offset index in the destination.
            -> Int -- ^ The number of elements to copy.
            -> Command
copyBuffer source dest srcOff destOff size
    = Command $ \queue -> clEnqueueCopyBuffer queue
            source dest
            (srcOff * eltWidth)
            (destOff * eltWidth)
            (size * eltWidth)
  where eltWidth = sizeOf (undefined :: a)


---------
-- TODO: make all this take Integral or CSize parameters.
class CopyTo a b where
    (=:) :: Storable e => a e -> b e -> Command

class BufferLike b where
    asSlice :: Storable e => b e -> Slice e

instance BufferLike Buffer where
    asSlice b = Slice 0 (bufferSize b) b

-- indexes are as elements.
-- there's some unnecessary operations, but whatever.
data Slice e = Slice {offsetS :: !Int,
                        sizeS :: !Int,
                        bufferS :: !(Buffer e)
                    }

instance Show (Slice e) where
    show s = "<Slice " ++ show (offsetS s, sizeS s) ++ ">"

instance BufferLike Slice where
    asSlice = id

slice :: Storable e => BufferLike b => Int -> Int -> b e -> Slice e
slice o s b = let
                b' = asSlice b
                n = bufferSize (bufferS b')
                o' = if o < 0 then offsetS b' else min n (offsetS b'+o)
                s' = if o' + s > n then n-o' else s
                in Slice {offsetS = o', sizeS = s', bufferS=bufferS b'}


instance BufferLike b => CopyTo b Ptr where
    b =: p = let s = asSlice b
                 in writeBuffer (bufferS s) NonBlocking
                        (offsetS s) (sizeS s) p

instance BufferLike b => CopyTo Ptr b where
    p =: b = let s = asSlice b
                 in readBuffer (bufferS s) NonBlocking
                        (offsetS s) (sizeS s) p

instance (BufferLike b1, BufferLike b2) => CopyTo b1 b2 where
    b1 =: b2 = let
        s1 = asSlice b1
        s2 = asSlice b2
        in if sizeS s1 /= sizeS s2
            then error "Mismatched sizes in buffer-to-buffer copy!"
            else copyBuffer (bufferS s2) (bufferS s1)
                    (offsetS s2) (offsetS s1) (sizeS s1)


foreign import ccall "&" clReleaseMemObject :: Releaser m

{#fun clReleaseMemObject as releaseMemObject
  { id `Ptr ()'
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

-- | Number of elements in the buffer.
bufferSize :: forall e . Storable e => Buffer e -> Int
bufferSize b = memSize b `div` sizeOf (undefined :: e)

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
    withMemObject = withBuffer
