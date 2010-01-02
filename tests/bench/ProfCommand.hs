{-# LANGUAGE FlexibleContexts #-}
module ProfCommand where

import OpenCL
import Data.List
import Control.Monad
import Data.Word
import Numeric

import Data.Array.MArray
import System.Random

average xs = foldl' (+) 0 xs / genericLength xs

-- Run a command multiple times, printing the timings and the average.
profCommand :: MonadQueue m => Int -> Command -> m ()
profCommand n f = do
    es <- replicateM n $ do
                e <- enqueue f
                waitForEvent e
                return e
    let getDuration e = fmap mkDouble
                            $ liftM2 (-) (getCommandEnd e) (getCommandStart e)
    liftIO $ do
    ts <- mapM getDuration es
    let showF x = showFFloat (Just 5) x ""
    putStrLn $ "Average duration: " ++ showF (average ts)
    putStrLn $ "range: (" ++ showF (minimum ts)
                ++ ", " ++ showF (maximum ts) ++ ")"

mkDouble :: Word64 -> Double
mkDouble x = fromIntegral x / 10^9

-- TODO: could be a lot more efficient.
randomA :: (Random e, MonadIO m, Ix i, MArray a e m)
                => (i,i) -> m (a i e)
randomA bounds = do
    a <- newArray_ bounds
    forM_ (range bounds) $ \i -> liftIO randomIO >>= writeArray a i
    return a
