{-# LANGUAGE FlexibleContexts #-}
module ProfCommand where

import OpenCL
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

average xs = foldl' (+) 0 xs / genericLength xs

-- Run a command multiple times, printing the timings and the average.
profCommand :: MonadQueue m => Int -> Command -> m ()
profCommand n f = do
    t0 <- liftIO getCurrentTime
    es <- waitForCommands $ replicate n f
    t1 <- liftIO getCurrentTime
    let showF x = showFFloat (Just 5) x ""
    liftIO $ do
    {-- let getDuration e = fmap mkDouble
                            $ liftM2 (-) (getCommandEnd e) (getCommandStart e)
    ts <- mapM getDuration es
    putStrLn $ "Average duration: " ++ showF (average ts)
    putStrLn $ "range: (" ++ showF (minimum ts)
                ++ ", " ++ showF (maximum ts) ++ ")"
    --}
    putStrLn $ "Timing: " ++ showF (realToFrac $ diffUTCTime t1 t0 :: Double)
    -- print $ map showF ts

mkDouble :: Word64 -> Double
mkDouble x = fromIntegral x / 10^9

-- TODO: could be a lot more efficient.
randomA :: (Random e, MonadIO m, Ix i, MArray a e m)
                => (i,i) -> m (a i e)
randomA bounds = do
    a <- newArray_ bounds
    forM_ (range bounds) $ \i -> liftIO randomIO >>= writeArray a i
    return a

randomCArray :: (Random e, Storable e, MonadQueue m, Ix i) => (i,i) -> m (CArray i e)
randomCArray bounds = liftIO $ randomA bounds >>= unsafeFreezeIOCArray
 
