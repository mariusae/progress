-- | Track & project progress of a process that is completing some
--   units of work.
module System.Console.Progress
  ( Progress
  , empty
  , update
  , projectTimeLeft
  , progress
  , drawProgress
  ) where

-- TODO: 
--
-- - have some generic String -> IO String for use eg. w/ interact to
--   automatically print progress in monad/arrow processes
--
-- - determine terminal width & proper escaping (eg. through vty), and
--   provide a function to deal with terminal output end-to-end


import Data.Time.Clock (diffUTCTime, UTCTime, getCurrentTime)
import Text.Printf (printf, PrintfArg)

data Progress a = Progress a a UTCTime Int

-- | The empty progress object.
empty :: (Num a) 
      => a                      -- ^ Total number of items to be completed.
      -> UTCTime                -- ^ Start time
      -> Progress a
empty total now = Progress 0 total now 0

-- | Update the progress object with completed items.
update :: (Num a) 
       => a                     -- ^ Number of items completed
       -> Progress a            -- ^ The progress object
       -> Progress a
update howmany (Progress count total time width) = 
  Progress (count + howmany) total time width

-- | Project the amount of time until completion in seconds.
projectTimeLeft :: (Integral a) 
                => UTCTime      -- ^ The current time
                -> Progress a   -- ^ The progress object
                -> Maybe a      -- ^ Projected time in seconds
projectTimeLeft when (Progress count total time _) =
  maybe Nothing (\rate -> Just $ floor $ (toRational left) / rate) rate
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

-- | The amount of progress made, in whole percents.
progress :: (Integral a) 
         => Progress a          -- ^ The progress object
         -> a                   -- ^ Whole percents
progress (Progress count total _ _)
  | total == 0 = 0
  | otherwise  = (100 * count) `div` total

-- | A string to be printed on the console with the current progress &
--   projected time to completion. The string contains terminal
--   escapes to erase previous output exceeding the current output
--   length.
drawProgress :: (PrintfArg a, Integral a)
             => UTCTime               -- ^ The current time
             -> Progress a            -- ^ The progress object
             -> (Progress a, String)  -- ^ A new progress object & the
                                      --   progress string
drawProgress when p@(Progress count total time width) =
  (Progress count total time width', str)
  where
    erase   = replicate width ' ' ++ replicate width '\x08'
    str'    = printf "%s done, remaining: %s" pct left
    width'  = length str'
    str     = erase ++ str'

    pct :: String
    pct = printf "%02d%%" $ progress p

    left :: String
    left = leftStr $ projectTimeLeft when p
    leftStr Nothing = "?"
    leftStr (Just s)
      | s < 60    = printf "%02ds" s
      | s < 3600  = printf "%02dm" (s `div` 60) ++ leftStr (Just (s `mod` 60))
      | otherwise = printf "%dh" (s `div` 3600) ++ leftStr (Just (s `mod` 3600))
