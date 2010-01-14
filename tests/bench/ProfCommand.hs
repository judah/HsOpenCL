{-# LANGUAGE FlexibleContexts #-}
module ProfCommand where

import System.HsOpenCL
import Data.List
import Control.Monad
import Data.Word
import Numeric

import Data.Array.MArray
import Data.Array.CArray.Base
import System.Random
import Foreign.Storable
import Data.Time.Clock
import Control.Concurrent
import Text.Printf
import Control.Applicative


-- Run a command multiple times, printing the timings and the average.
profCommand :: MonadQueue m => Int -> Command -> m ()
profCommand n f = do
    (es,t) <- timed $ waitForCommands $ replicate n f
    liftIO $ printf "Ran %d iterations.\n" n
    liftIO $ printf "Average time (system clock): %.5f\n" $ t/toEnum n
    t' <- liftIO $ eventDurations es
    liftIO $ printf "Average time (event timing): %.5f\n" $ t'

eventDurations :: [Event] -> IO Double
eventDurations es = do
    ts <- forM es $ \e -> mkDouble <$> liftM2 (-) (getCommandEnd e) (getCommandStart e)
    return $ average ts
  where
    average xs = foldl' (+) 0 xs / genericLength xs
    mkDouble :: Word64 -> Double
    mkDouble x = fromIntegral x / 10^9

timed :: MonadIO m => m a -> m (a,Double)
timed f = do
    t0 <- liftIO getCurrentTime
    x <- f
    t1 <- liftIO getCurrentTime
    return (x, realToFrac $ diffUTCTime t1 t0)


------------------------
-- Some useful utilities
-- TODO: could be a lot more efficient.
randomA :: (Random e, MonadIO m, Ix i, MArray a e m)
                => (i,i) -> m (a i e)
randomA bounds = do
    a <- newArray_ bounds
    forM_ (range bounds) $ \i -> liftIO randomIO >>= writeArray a i
    return a

randomCArray :: (Random e, Storable e, MonadQueue m, Ix i) => (i,i) -> m (CArray i e)
randomCArray bounds = liftIO $ randomA bounds >>= unsafeFreezeIOCArray
 
putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print


