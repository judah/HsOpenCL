module System.HsOpenCL.Internal.Types where

#include <OpenCL/OpenCL.h>

import Foreign
import Foreign.C
import Control.Applicative
import Control.Monad
import Control.Exception

import System.HsOpenCL.Error
import System.HsOpenCL.Internal.C2HS

import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Unsafe(unsafePackMallocCString)

-- I'm assuming that types like cl_device_id are actually pointers,
-- so they can be passed around by the FFI.
-- This is true on Apple systems, but I'm not sure how portable it is.

type Releaser a_ = FunPtr (Ptr a_ -> IO ())

-- TODO: does this have overflow?
cEnum :: (Enum a, Enum b) => a -> b
cEnum = toEnum . fromEnum

cuintToInt :: CUInt -> Int
cuintToInt = cEnum

newData :: (ForeignPtr a_ -> a) -> Releaser a_
                    -> (Ptr () -> IO a)
newData construct release p = construct <$> newForeignPtr release (castPtr p)



-- Note that unlike the others, DeviceID and PlatformID have no way to be
-- released, so it's just a pointer.
data DeviceID_
-- | An OpenCL device, such as a GPU or CPU.
newtype DeviceID = DeviceID {_deviceIDPtr :: Ptr DeviceID_}
deviceIDPtr :: DeviceID -> Ptr ()
deviceIDPtr (DeviceID p) = castPtr p

withDeviceIDs :: [DeviceID] -> ((CUInt, Ptr (Ptr ())) -> IO a) -> IO a
withDeviceIDs devices f = withArrayLen (map _deviceIDPtr devices)
                            $ \n ps -> f (toEnum n, castPtr ps)

data PlatformID_
newtype PlatformID = PlatformID {_platformIDPtr :: Ptr PlatformID_}
platformIDPtr :: PlatformID -> Ptr ()
platformIDPtr (PlatformID p) = castPtr p

------------
data Context_
newtype Context = Context (ForeignPtr Context_)
withContext :: Context -> (Ptr () -> IO a) -> IO a
withContext (Context p) f = withForeignPtr p $ f . castPtr

newContext :: Ptr () -> IO Context
newContext = newData Context clReleaseContext
foreign import ccall "&" clReleaseContext :: Releaser Context_

{#fun clRetainContext
  { id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

-- Note: be careful of races if the child is a ForeignPtr.  (See clQueueContext).
retainedCLContext :: Ptr () -> IO Context
retainedCLContext p = clRetainContext p >> newContext' p

newContext' :: Ptr () -> IO Context
newContext' = newData Context clReleaseContext

-- Being careful of race conditions:
-- The foreignptr points back to the context, and if it's GC'd
-- it could cause the context to be released prematurely.
-- So, make sure the ForeignPtr stays alive long enough for us to retain
-- the context.
retainedContextInfo :: ForeignPtr a -> Ptr () -> IO Context
retainedContextInfo child p = withForeignPtr child $ \_ -> do
    clRetainContext p
    newContext p
------------

data CommandQueue_
newtype CommandQueue = CommandQueue (ForeignPtr CommandQueue_)
withCommandQueue :: CommandQueue -> (Ptr () -> IO a) -> IO a
withCommandQueue (CommandQueue p) f = withForeignPtr p $ f . castPtr

data Program_
newtype Program = Program (ForeignPtr Program_)
withProgram :: Program -> (Ptr () -> IO a) -> IO a
withProgram (Program p) f = withForeignPtr p $ f . castPtr

retainedProgram :: Ptr () -> IO Program
retainedProgram p = clRetainProgram p >> newProgram p

{#fun clRetainProgram
  { id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

foreign import ccall "&" clReleaseProgram :: Releaser Program_

newProgram :: Ptr () -> IO Program
newProgram = newData Program clReleaseProgram


data Kernel_
newtype Kernel = Kernel (ForeignPtr Kernel_)
withKernel :: Kernel -> (Ptr () -> IO a) -> IO a
withKernel (Kernel p) f = withForeignPtr p $ f . castPtr

data Event_
newtype Event = Event (ForeignPtr Event_)
withEvent :: Event -> (Ptr () -> IO a) -> IO a
withEvent (Event p) f = withForeignPtr p $ f . castPtr

withEvents :: [Event] -> ((CUInt, Ptr (Ptr ())) -> IO a) -> IO a
withEvents [] g = g (0,nullPtr) -- required by OpenCL spec
withEvents es g = withMany withEvent es $ \ps ->
                    withArrayLen ps $ \len p_ps -> g (toEnum len, p_ps)

----------------
-- Commands

-- | An action which can be enqueued and run on an OpenCL device;
-- for example, reading/writing host memory or running a program kernel.
newtype Command = Command {runCommand :: CommandQueue ->
                            [Event] -> EventPtr -> IO (IO ())}

-- | Low-level function to help create 'Command's.
mkCommand :: (CommandQueue -> [Event] -> EventPtr -> IO ())
                    -> Command
mkCommand f = Command $ \q es e -> f q es e >> return (return ())


-----------------------------
-- Memory
-- 
-- Note: cl_mem's aren't retained when they're set as kernel arguments.
-- Rather, they're retained when the kernel starts running, and released
-- once it's finished.
-- Thus, in OpenCL.Kernel we set and launch the kernel all within the same call
-- withBuffer, so it can never be freed accidentally.
data Buffer_

-- | A region of memory on an OpenCL device.
newtype Buffer a = Buffer (Ptr Buffer_)
withBuffer :: Buffer a -> (Ptr () -> IO b) -> IO b
withBuffer (Buffer p) = bracket_ (retainMemObject p) (releaseMemObject p)
                            . ($ castPtr p)


foreign import ccall "&" clRetainMemObject :: Releaser m

{#fun clRetainMemObject as retainMemObject
  { castPtr `Ptr m'
  } -> `Int' checkSuccess*-
#}

foreign import ccall "&" clReleaseMemObject :: Releaser m

{#fun clReleaseMemObject as releaseMemObject
  { castPtr `Ptr m'
  } -> `Int' checkSuccess*-
#}


--------------------
-- Most of the types have a clGet*Info function following the same
-- template;
-- here's some boilerplace.
--
-- Note this code is somewhat platform-dependent.

-- We should have c2hs export them so that the last parameters look
-- like:
type GetInfoFunc = Int -> Ptr () -> IO Int

storableInfo :: forall a . Storable a => GetInfoFunc -> IO a
storableInfo getInfo = alloca $ \p -> do
    retSize <- getInfo size (castPtr p)
    -- Double-check we got the types correct:
    -- Note that if a bitfield has no values set, the retSize may be 0.
    when (retSize /= size && retSize > 0)
        $ error $ 
            "storableInfo: Incorrect property size: expected " ++ show size
                    ++ ", got: " ++ show retSize
    peek p
  where size = sizeOf (undefined :: a)

class Property a where
    getProp :: GetInfoFunc -> IO a

getPureProp :: Property a => GetInfoFunc -> a
getPureProp = unsafePerformIO . getProp

instance Property CUInt where
    getProp = storableInfo

instance Property CULLong where
    getProp = storableInfo

instance Property CInt where
    getProp = storableInfo

instance Property CSize where
    getProp = storableInfo

-- TODO: This is probably wrong.
instance Property Bool where
    getProp getInfo = (/=0) <$> (storableInfo getInfo :: IO {#type cl_bool #})

instance Property (Ptr a) where
    getProp = storableInfo

instance Property Word32 where
    getProp = storableInfo

instance Property Word64 where
    getProp = storableInfo

instance Enum a => Property (a -> Bool) where
    getProp getInfo = do
        bitmask :: CULLong <- storableInfo getInfo
        return (containsBitMask bitmask)

getFlags :: Enum a => GetInfoFunc -> [a] -> IO [a]
getFlags getInfo fs = getProp getInfo >>= return . flip filter fs

getPureFlags :: Enum a => GetInfoFunc -> [a] -> [a]
getPureFlags getInfo = unsafePerformIO . getFlags getInfo

instance Storable a => Property [a] where
    getProp getInfo = do
        retSize <- getInfo 0 nullPtr
        getArrayN (retSize `div` sizeOf (undefined :: a)) getInfo

-- NOTE: the String and ByteString instances are useful ONLY for reading
-- null-terminated data.
-- To read binary data, see e.g. createProgramWithBinary.
instance Property String where
    getProp getInfo = unpack <$> getProp getInfo

instance Property ByteString where
    getProp getInfo = do
        size <- getInfo 0 nullPtr
        p <- mallocArray size
        getInfo size (castPtr p)
        unsafePackMallocCString p

getArrayN :: forall a . Storable a => Int -> GetInfoFunc -> IO [a]
getArrayN numEntries getInfo = allocaArray numEntries $ \p -> do
        getInfo numBytes (castPtr p)
        peekArray numEntries p
  where
    numBytes = numEntries * sizeOf (undefined :: a)

-- TODO: more objects should use this.
getObjArray :: GetInfoFunc -> IO [Ptr a]
getObjArray getInfo = do
    n <- getInfo 0 nullPtr
    allocaArray n $ \ps -> do
        n' <- getInfo n (castPtr ps)
        peekArray n' ps

newtype EventPtr = EventPtr {eventPtr :: Ptr (Ptr ())}
