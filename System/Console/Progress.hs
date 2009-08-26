{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Console.Progress
  ( Progress
  , empty
  , update
  , project
  , pctDone
  , draw
  ) where

import Data.Time.Clock (diffUTCTime, UTCTime, getCurrentTime)
import Text.Printf (printf, PrintfArg)
import Control.Monad.State (StateT, runStateT, MonadState
                           , MonadIO, modify, get, liftIO)

data Progress a = Progress a a UTCTime

empty :: (Num a) => a -> UTCTime -> Progress a
empty total now = Progress 0 total now

update :: (Num a) => a -> Progress a -> Progress a
update howmany (Progress count total time) = 
  Progress (count + howmany) total time

project :: (Integral a) => UTCTime -> Progress a -> Maybe a
project when (Progress count total time) =
  if rate > 0
     then Just $ floor $ (toRational left) / rate
     else Nothing
  where
    left     = total - count
    rate     = (toRational count) / diffSecs
    diffSecs = toRational $ diffUTCTime when time

pctDone :: (Integral a) => Progress a -> a
pctDone (Progress count total _) = (100 * count) `div` total

draw :: (PrintfArg a, Integral a) => UTCTime -> Progress a -> String
draw when p@(Progress count total time) =
  printf "%s done, remaining: %s" pct left :: String
  where 
    pct :: String
    pct = printf "%02d%%" $ pctDone p

    left :: String
    left = leftStr $ project when p
    leftStr Nothing = "?"
    leftStr (Just s)
      | s < 60    = printf "%02ds" s
      | s < 3600  = printf "%02dm" (s `div` 60) ++ leftStr (Just (s `mod` 60))
      | otherwise = printf "%dh" (s `div` 3600) ++ leftStr (Just (s `mod` 3600))

newtype ProgressM a = ProgressM (StateT (Progress Integer) IO a)
                      deriving (Monad, MonadIO, MonadState (Progress Integer))

reset :: Integer -> ProgressM ()
reset howmany = do
  t <- liftIO $ getCurrentTime
  modify $ \p -> empty howmany t

makeProgress :: Integer -> ProgressM ()
makeProgress howmuch = modify $ update howmuch

displayProgress :: ProgressM ()
displayProgress = do
  t <- liftIO $ getCurrentTime
  get >>= liftIO . putStr . draw t

runProgress :: Integer -> ProgressM a -> IO a
runProgress howmany (ProgressM action) =
  getCurrentTime >>= return . empty howmany >>= 
  runStateT action >>= \(a, _) -> return a
