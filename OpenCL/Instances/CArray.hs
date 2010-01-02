module OpenCL.Instances.CArray(
        asCArray,
        asIOCArray,
        ) where

-- helper instances for using CArrays.

import OpenCL.Kernel
import OpenCL.Memory
import OpenCL.CommandQueue

import Data.Array.CArray
import Data.Array.CArray.Base
import Control.Concurrent
import Foreign.ForeignPtr

-- Give the type-checker some hints, since functions like newArray
-- are polymorphic.  This seems less messy than a bunch of ScopedTypeVariable
-- bindings.
asCArray :: CArray i a -> CArray i a
asCArray = id

asIOCArray :: m (IOCArray i a) -> m (IOCArray i a)
asIOCArray = id

enqueueWithFree :: IO a -> Command -> CommandQueue -> [Event] -> IO Event
enqueueWithFree act f q es = do
    e <- runCommand f q es
    forkIO $ myWait e >> act >> return ()
    return e

myWait :: Event -> IO ()
myWait e = do
    stat <- getEventCommandExecutionStatus e
    case stat of
        Complete -> return ()
        _ -> threadDelay time >> myWait e

time = 10000
iocarraySize (IOCArray _ _ n _) = n

instance (Ix i) => CopyTo (IOCArray i) Slice where
    a =: b
        | iocarraySize a < sizeS b = error "Copying too big!"
        | otherwise = Command $ \q es -> withIOCArray a $ \p ->
                enqueueWithFree (touchIOCArray a) (p =: b) q es

instance (Ix i) => CopyTo Slice (IOCArray i) where
    b =: a
        | iocarraySize a > sizeS b = error "Copying too big!"
        | otherwise = Command $ \q es -> withIOCArray a $ \p ->
                enqueueWithFree (touchIOCArray a) (b =: p) q es

-- TODO: is this actually safe w/ the touching?
instance (Ix i) => CopyTo Slice (CArray i) where
    b =: a = case toForeignPtr a of
            (n,fp)
                | n > sizeS b -> error "Copying too big!"
                | otherwise -> Command $ \q es -> withForeignPtr fp $ \p ->
                    enqueueWithFree (touchForeignPtr fp) (b =: p) q es

instance Ix i => CopyTo (IOCArray i) Buffer where
    a =: b = a =: asSlice b

instance Ix i => CopyTo Buffer (IOCArray i) where
    b =: a = asSlice b =: a

instance Ix i => CopyTo Buffer (CArray i) where
    b =: a = asSlice b =: a
