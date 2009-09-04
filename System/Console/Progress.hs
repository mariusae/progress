{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Console.Progress
  ( Progress
  , empty
  , update
  , projectTimeLeft
  , pctDone
  , drawProgressBar
  -- * The ProgressM monad.
  , ProgressM
  , runProgress
  , resetProgress
  , displayProgressBar
  ) where

import Data.Time.Clock (diffUTCTime, UTCTime, getCurrentTime)
import Text.Printf (printf, PrintfArg)
import Control.Monad.State (StateT, runStateT, MonadState
                           , MonadIO, modify, get, put, liftIO)

data Progress a = Progress a a UTCTime Int

empty :: (Num a) => a -> UTCTime -> Progress a
empty total now = Progress 0 total now 0

update :: (Num a) => a -> Progress a -> Progress a
update howmany (Progress count total time width) = 
  Progress (count + howmany) total time width

projectTimeLeft :: (Integral a) => UTCTime -> Progress a -> Maybe a
projectTimeLeft when (Progress count total time _) =
  maybe Nothing (\r -> Just $ floor $ (toRational left) / r) rate
  where
    left     = total - count
    diffSecs = toRational $ diffUTCTime when time
    rate'    = if diffSecs > 0 
                 then Just $ (toRational count) / diffSecs
                 else Nothing
    rate     = maybe 
                 Nothing 
                 (\r -> if r > 0 then (Just r) else Nothing) 
                 rate'

pctDone :: (Integral a) => Progress a -> a
pctDone (Progress count total _ _)
  | total == 0 = 0
  | otherwise  = (100 * count) `div` total

drawProgressBar :: (PrintfArg a, Integral a) => 
                   UTCTime 
                -> Progress a 
                -> (Progress a, String)
drawProgressBar when p@(Progress count total time width) =
  (Progress count total time width', str)
  where

    erase   = replicate width ' ' ++ replicate width '\x08'
    str'    = printf "%s done, remaining: %s" pct left
    width'  = length str'
    str     = erase ++ str'

    pct :: String
    pct = printf "%02d%%" $ pctDone p

    left :: String
    left = leftStr $ projectTimeLeft when p
    leftStr Nothing = "?"
    leftStr (Just s)
      | s < 60    = printf "%02ds" s
      | s < 3600  = printf "%02dm" (s `div` 60) ++ leftStr (Just (s `mod` 60))
      | otherwise = printf "%dh" (s `div` 3600) ++ leftStr (Just (s `mod` 3600))

newtype ProgressM a = ProgressM (StateT (Progress Integer) IO a)
                      deriving (Monad, MonadIO, MonadState (Progress Integer))

resetProgress :: Integer -> ProgressM ()
resetProgress howmany = do
  t <- liftIO $ getCurrentTime
  modify $ \p -> empty howmany t

makeProgress :: Integer -> ProgressM ()
makeProgress howmuch = modify $ update howmuch

displayProgressBar :: ProgressM ()
displayProgressBar = do
  t <- liftIO $ getCurrentTime
  d <- get
  let (d', str) = drawProgressBar t d
  put d'
  liftIO $ putStr str

runProgress :: Integer -> ProgressM a -> IO a
runProgress howmany (ProgressM action) =
  getCurrentTime >>= return . empty howmany >>= 
  runStateT action >>= \(a, _) -> return a
