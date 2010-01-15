module System.HsOpenCL.Instances.CArray(
        asCArray,
        asIOCArray,
        copyToCArray
        ) where

-- helper instances for using CArrays.

import System.HsOpenCL.Kernel
import System.HsOpenCL.Memory
import System.HsOpenCL.CommandQueue
import System.HsOpenCL.MonadQueue

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
asCArray :: CArray i a -> CArray i a
asCArray = id

asIOCArray :: m (IOCArray i a) -> m (IOCArray i a)
asIOCArray = id

time = 10000
iocarraySize (IOCArray _ _ n _) = n

instance (Ix i) => CopyTo (IOCArray i) Slice where
    a =: b
        | iocarraySize a < sizeS b = error "Copying too big!"
        | otherwise = Command $ \q es ep -> withIOCArray a $ \p ->
                (>>touchIOCArray a) <$> 
                        runCommand (p =: b) q es ep

instance (Ix i) => CopyTo Slice (IOCArray i) where
    b =: a
        | iocarraySize a > sizeS b = error "Copying too big!"
        | otherwise = Command $ \q es ep -> withIOCArray a $ \p -> do
                (>>touchIOCArray a) <$> 
                    runCommand (b =: p) q es ep

-- TODO: is this actually safe w/ the touching?
instance (Ix i) => CopyTo Slice (CArray i) where
    b =: a = case toForeignPtr a of
            (n,fp)
                | n > sizeS b -> error "Copying too big!"
                | otherwise -> Command $ \q es ep -> withForeignPtr fp $ \p -> do
                    (>>touchForeignPtr fp)
                        <$> runCommand (b =: p) q es ep

instance Ix i => CopyTo (IOCArray i) Buffer where
    a =: b = a =: asSlice b

instance Ix i => CopyTo Buffer (IOCArray i) where
    b =: a = asSlice b =: a

instance Ix i => CopyTo Buffer (CArray i) where
    b =: a = asSlice b =: a

copyToCArray :: (Ix i, Storable e, MonadQueue m, CopyTo (IOCArray i) b)
                    => (i,i) -> b e -> m (CArray i e)
copyToCArray bounds b = do
    a <- liftIO $ newArray_ bounds
    waitForCommands_ [ a =: b]
    liftIO $ unsafeFreezeIOCArray a
