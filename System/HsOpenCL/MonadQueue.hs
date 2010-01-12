module System.HsOpenCL.MonadQueue(QueueT(..)
                        , QueueIO
                        , MonadBracket(..)
                        , MonadQueue(..)
                        , module Control.Monad.Trans
                        ) where

import System.HsOpenCL.Internal.Types

import Control.Monad.Trans
import Data.Ix
import Data.Array.Base(MArray(..))

-- TODOs:
-- - does calling API each time we want a device or context cause a slowdown?
-- - multiple devices in a context?

newtype QueueT m a = QueueT {runQueueT :: CommandQueue -> m a}

type QueueIO = QueueT IO

instance Functor m => Functor (QueueT m) where
    fmap f g = QueueT $ fmap f . runQueueT g

-- TODO: Applicative definition

instance Monad m => Monad (QueueT m) where
    return x = QueueT $ const $ return x
    f >>= g = QueueT $ \q -> runQueueT f q >>= flip runQueueT q . g

instance MonadTrans QueueT where
    lift = QueueT . const

instance MonadIO m => MonadIO (QueueT m) where
    liftIO = lift . liftIO

class MonadIO m => MonadBracket m where
    liftIOBracket :: (forall b . (a -> IO b) -> IO b)
                            -> (a -> m b) -> m b

instance MonadBracket IO where
    liftIOBracket = id

instance MonadBracket m => MonadBracket (QueueT m) where
    liftIOBracket wrap f = QueueT $ \queue ->
           liftIOBracket wrap $ \x -> runQueueT (f x) queue

class MonadBracket m => MonadQueue m where
    getQueue :: m CommandQueue

instance MonadBracket m => MonadQueue (QueueT m) where
    getQueue = QueueT return

-- TODO: see if INLINEs are necessary
instance MArray a e m => MArray a e (QueueT m) where
    getBounds = lift . getBounds
    getNumElements = lift . getNumElements
    newArray bounds e = lift $ newArray bounds e
    newArray_ = lift . newArray_
    unsafeNewArray_ = lift . unsafeNewArray_
    unsafeRead a i = lift $ unsafeRead a i
    unsafeWrite a i x = lift $ unsafeWrite a i x

