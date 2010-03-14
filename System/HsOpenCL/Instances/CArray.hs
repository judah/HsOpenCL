-- | This module exports instances of 'CopyTo' for copying from (immutable) 'CArray's to 'Buffer's,
-- and between 'Buffer's and (mutable) 'IOCArray's.
--
-- It also contains a few helper functions for integrating 'CArray's with OpenCL.
module System.HsOpenCL.Instances.CArray(
        asCArray,
        asIOCArray,
        copyToCArray
        ) where

-- helper instances for using CArrays.

import System.HsOpenCL.Kernel
import System.HsOpenCL.Memory
import System.HsOpenCL.CommandQueue

import Control.Monad.Trans
import Data.Array.CArray
import Data.Array.IOCArray
import Data.Array.CArray.Base
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Applicative

-- Give the type-checker some hints, since functions like newArray
-- are polymorphic.  This seems less messy than a bunch of ScopedTypeVariable
-- bindings.

-- | Forces the type of an 'Array' instance to be 'CArray'.
asCArray :: CArray i a -> CArray i a
asCArray = id

-- | Forces the type of an 'MArray' instance to be 'IOCArray'.
asIOCArray :: m (IOCArray i a) -> m (IOCArray i a)
asIOCArray = id

iocarraySize (IOCArray _ _ n _) = n

instance (BufferLike b, Ix i) => CopyTo (IOCArray i) b where
    IOCArray _ _ n fp =: b
        = SlicedPtr {ptrFPtr=fp, ptrOffset=0, ptrLength=n} =: asSlice b

instance (BufferLike b, Ix i) => CopyTo b (IOCArray i) where
    b =: IOCArray _ _ n fp
        = asSlice b =: SlicedPtr {ptrFPtr=fp, ptrOffset=0, ptrLength=n}

instance (BufferLike b, Ix i) => CopyTo b (CArray i) where
    b =: CArray i j n fp = asSlice b =: IOCArray i j n fp

-- | Create a new (immutable) 'CArray' from the contents of a 'Buffer' or 'Slice'.
copyToCArray :: (Ix i, Storable e, MonadQueue m, CopyTo (IOCArray i) b)
                    => (i,i) -> b e -> m (CArray i e)
copyToCArray bounds b = do
    a <- liftIO $ newArray_ bounds
    waitForCommands_ [ a =: b]
    liftIO $ unsafeFreezeIOCArray a
