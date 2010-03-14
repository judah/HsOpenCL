-- | This module exports instances of 'CopyTo' for the @vector@ package (specifically,
-- "Data.Vector.Storable").  Specifically,
-- it provides instances for copying data from 'Vector's to 'Buffer's, 
-- and for copying data between 'IOVector's and 'Buffer's.
module System.HsOpenCL.Instances.Vector(
        copyToVector
        ) where

import System.HsOpenCL.Memory
import System.HsOpenCL.CommandQueue

import Data.Vector.Storable as V
import Data.Vector.Storable.Mutable as MV
import Data.Vector.Generic (unsafeFreeze)

instance BufferLike b => CopyTo IOVector b where
    v =: b = sp =: asSlice b
      where
        (fp,offset,len) = MV.unsafeToForeignPtr v
        sp = SlicedPtr {ptrFPtr=fp, ptrOffset=offset, ptrLength=len}

instance BufferLike b => CopyTo b IOVector where
    b =: v = asSlice b =: sp
      where
        (fp,offset,len) = MV.unsafeToForeignPtr v
        sp = SlicedPtr {ptrFPtr=fp, ptrOffset=offset, ptrLength=len}

instance BufferLike b => CopyTo b Vector where
    b =: v = asSlice b =: sp
      where
        (fp,offset,len) = V.unsafeToForeignPtr v
        sp = SlicedPtr {ptrFPtr=fp, ptrOffset=offset, ptrLength=len}

copyToVector :: (Storable e, MonadQueue m, BufferLike b)
                    => b e -> m (Vector e)
copyToVector m = do
    let b = asSlice m
    mv <- liftIO $ new (sizeS b)
    waitForCommands [mv =: b]
    liftIO $ unsafeFreeze mv
