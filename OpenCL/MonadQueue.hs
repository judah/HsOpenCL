module OpenCL.MonadQueue(QueueT(..)
                        , QueueIO
                        , MonadBracket(..)
                        , MonadQueue(..)
                        , module Control.Monad.Trans
                        ) where

import OpenCL.Internal.Types

import Control.Monad.Trans

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

instance MonadIO m => MonadIO (QueueT m) where
    liftIO = QueueT . const . liftIO


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
