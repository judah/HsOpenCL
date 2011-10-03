module System.HsOpenCL.Memory(
                -- * Buffers
                Buffer,
                castBuffer,
                bufferSize,
                mallocBuffer,
                freeBuffer,
                allocaBuffer,
                -- * Reading, writing and copying
                CopyTo(..),
                BufferLike(..),
                Slice(),
                slice,
                sizeS,
                SlicedPtr(..),
                copyToVector,
                -- ** Buffer operations
                -- | This section provides an API for buffer operations which is closer
                -- to the actual OpenCL API.
                IsBlocking(..),
                readBuffer,
                writeBuffer,
                copyBuffer,
                -- * Properties
                MemObject(),
                MemAccessFlag(..),
                MemInitFlag(..),
                memFlags,
                memSize,
                memContext,
                -- getMemReferenceCount,
                ) where

#include <OpenCL/OpenCL.h>
import System.HsOpenCL.Internal.Types
import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.CommandQueue
import Control.Applicative
import Control.Exception (bracket)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV


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

-- | Allocate memory on the device associated with this queue.  The returned 'Buffer'
-- must later be released with 'freeBuffer'.
mallocBuffer :: forall a m . (Storable a, MonadQueue m)
        => MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> m (Buffer a)
mallocBuffer memAccess memInit size = do
    cxt <- getContext
    liftIO $ newBuffer' cxt memAccess memInit size

-- | Release the given 'Buffer'.  The OpenCL runtime will delete it
-- once all 'Command's using the 'Buffer' have completed.
freeBuffer :: MonadIO m => Buffer a -> m ()
freeBuffer b = liftIO $ withBuffer b releaseMemObject

newBuffer' :: forall a . (Storable a)
        => Context -> MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> IO (Buffer a)
newBuffer' context memAccess hostPtr size
    = Buffer <$> clCreateBuffer context flags (size * eltSize) p'
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

-- | Allocate memory on the device associated with this queue, and execute the given
-- action.  The 'Buffer' will be released once the action has completed and all 'Command's
-- associated with it have finished.
-- 
-- Note that it is unsafe to use the 'Buffer' after this function has completed.
allocaBuffer :: (Storable a, MonadQueue m) => MemAccessFlag -> MemInitFlag a
            -> Int -- ^ The number of elements in the buffer.
            -> (Buffer a -> m b) -> m b
allocaBuffer memAccess hostPtr size f = do
    context <- getContext
    liftIOBracket (bracket
                        (newBuffer' context memAccess hostPtr size)
                        freeBuffer)
        f

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
  , eventPtr `EventPtr'
  } -> `Int' checkSuccess*-
#}

readBuffer :: forall a . Storable a
        => Buffer a -> IsBlocking
                                -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                    -> Ptr a -> Command
readBuffer mem block offset size p
    = mkCommand $ \queue -> clEnqueueReadBuffer queue mem block
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
  , eventPtr `EventPtr'
  } -> `Int' checkSuccess*-
#}

writeBuffer :: forall a . Storable a => Buffer a -> IsBlocking
                                -> Int -- ^ The offset index.
                                -> Int -- ^ The number of elements to copy.
                                -> Ptr a -> Command
writeBuffer mem blocking offset size p
    = mkCommand $ \queue -> clEnqueueWriteBuffer queue mem blocking
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
  , eventPtr `EventPtr'
  } -> `Int' checkSuccess*-
#}

copyBuffer :: forall a . Storable a
    => Buffer a -- ^ The source buffer
            -> Buffer a -- ^ The destination buffer.
            -> Int -- ^ The offset index in the source buffer.
            -> Int -- ^ The offset index in the destination.
            -> Int -- ^ The number of elements to copy.
            -> Command
copyBuffer source dest srcOff destOff size
    = mkCommand $ \queue -> clEnqueueCopyBuffer queue
            source dest
            (srcOff * eltWidth)
            (destOff * eltWidth)
            (size * eltWidth)
  where eltWidth = sizeOf (undefined :: a)


---------
-- TODO: make all this take Integral or CSize parameters.

{- | 
 Reads, writes and copies between device and host memory can all be performed using
the 'CopyTo' class.

For example, if @a :: Buffer Float@ and @c :: Ptr Float@, then @waitForCommand (a := c)@
copies @bufferSize a@ elements (i.e., @Float@s) from @c@ to @a@.

This class can also be used to copy to/from subregions of device memory.  For example,
@waitForCommand (slice 2 10 a =: slice 0 10 b)@ copies the first 10 elements of @b@ into
indices @[2..11]@ of @a@.

All 'CopyTo' operations are unblocking.  As a result, when 'QueueOutOfOrderExecModeEnable'
has been set, the runtime may copy data simultaneously or out of order from other
'Command's.  

The module "System.HsOpenCL.Instances.CArray" exports instances for copying to and from
'CArray's, which may be more convenient to use than pointers.
-}
class CopyTo a b where
    -- | A copy between host and device memory, or between two device memory objects.
    (=:) :: Storable e => a e -> b e -> Command

-- | This class represents objects of which we can select a subregion; namely, 'Buffer'
-- and 'Slice'.
class BufferLike b where
    asSlice :: Storable e => b e -> Slice e

instance BufferLike Buffer where
    asSlice b = Slice 0 (bufferSize b) b

-- | A subregion of a 'Buffer'.
data Slice e = Slice {offsetS :: !Int,
                        sizeS :: !Int,
                        bufferS :: !(Buffer e)
                    }

instance Show (Slice e) where
    show s = "<Slice " ++ show (offsetS s, sizeS s) ++ ">"

instance BufferLike Slice where
    asSlice = id

-- | Select a subregion of the given 'Buffer' or 'Slice'.
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

-- | A slice of the data contained in a ForeignPtr.  The 'CopyTo' instance for this type
-- ensures that the 'ForeignPtr' will be retained until the copy has completed.
-- 
-- An error will be thrown when copying between a SlicedPtr and a Buffer of unequal sizes.
data SlicedPtr a = SlicedPtr {ptrFPtr :: ForeignPtr a, ptrOffset :: Int, ptrLength :: Int}

withSlicedPtr :: Storable a => SlicedPtr a -> (Ptr a -> IO b) -> IO b
withSlicedPtr sp f = withForeignPtr (ptrFPtr sp) $ \p -> f $ advancePtr p (ptrOffset sp)

touchSlicedPtr :: SlicedPtr a -> IO ()
touchSlicedPtr = touchForeignPtr . ptrFPtr

-- Note we use asSlice in these instances to prevent overlapping/incoherent instances.
instance BufferLike b => CopyTo b SlicedPtr where
    b =: sp
        | ptrLength sp /= sizeS b' = error "Mismatched sizes in host-to-device copy!"
        | otherwise = Command $ \q es ep -> withSlicedPtr sp $ \p ->
                        (>> touchSlicedPtr sp) <$>
                            runCommand (b' =: p) q es ep
      where b' = asSlice b

instance BufferLike b => CopyTo SlicedPtr b where
    sp =: b
        | ptrLength sp /= sizeS b' = error "Mismatched sizes in device-to-host copy!"
        | otherwise = Command $ \q es ep -> withSlicedPtr sp $ \p ->
                        (>> touchSlicedPtr sp) <$>
                            runCommand (p =: b') q es ep
      where b' = asSlice b


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
memFlags m = (accessFlag, memInit)
  where
    flagExists = getPureProp (getMemInfo m CLMemFlags)
    accessFlag
        | flagExists CLMemReadOnly_ = MemReadOnly
        | flagExists CLMemWriteOnly_ = MemWriteOnly
        | otherwise = MemReadWrite
    getPtrProp constr = constr $ getPureProp (getMemInfo m CLMemHostPtr)
    memInit
        | flagExists CLMemUseHostPtr_   = getPtrProp UseHostPtr
        | flagExists CLMemCopyHostPtr_
            = if flagExists CLMemAllocHostPtr_ then getPtrProp CopyAllocHostPtr
                    else getPtrProp CopyHostPtr
        | flagExists CLMemAllocHostPtr_ = AllocHostPtr
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

--------
--  Opaque class for cl_mem
--
-- There's a lot of ways we could go with the mem objects;
-- but this API shouldn't add too much to what's already in OpenCL.

-- | A memory region on the device.  
class MemObject m where
    withMemObject :: m -> (Ptr () -> IO a) -> IO a

instance MemObject (Buffer a) where
    withMemObject = withBuffer


--------------------
-- Instances for Vector:
instance BufferLike b => CopyTo MV.IOVector b where
    v =: b = sp =: asSlice b
      where
        (fp,offset,len) = MV.unsafeToForeignPtr v
        sp = SlicedPtr {ptrFPtr=fp, ptrOffset=offset, ptrLength=len}

instance BufferLike b => CopyTo b MV.IOVector where
    b =: v = asSlice b =: sp
      where
        (fp,offset,len) = MV.unsafeToForeignPtr v
        sp = SlicedPtr {ptrFPtr=fp, ptrOffset=offset, ptrLength=len}

instance BufferLike b => CopyTo b V.Vector where
    b =: v = asSlice b =: sp
      where
        (fp,offset,len) = V.unsafeToForeignPtr v
        sp = SlicedPtr {ptrFPtr=fp, ptrOffset=offset, ptrLength=len}

copyToVector :: (Storable e, MonadQueue m, BufferLike b)
                    => b e -> m (V.Vector e)
copyToVector m = do
    let b = asSlice m
    mv <- liftIO $ MV.new (sizeS b)
    waitForCommands [mv =: b]
    liftIO $ V.unsafeFreeze mv
