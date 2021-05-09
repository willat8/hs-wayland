{-# LANGUAGE OverloadedStrings #-}

module Myth.Alert (getBabyMonitorStatus) where
import Myth.Common
import qualified Data.ByteString as B
import Data.Bits.Bitwise

getBabyMonitorStatus :: IO (Int)
getBabyMonitorStatus = do
    poem <- getUrl "http://poem:4714/status"
    bard <- getUrl "http://bard:4714/status"
    return . fromListLE $ [(count "state: RUNNING" poem) == 8, (count "state: RUNNING" bard) == 2]

-- Count the number of substrings in a ByteString
count "" _      = 0
count search bs = if B.null t then 0 else 1 + count search (B.drop (B.length search) t)
    where (h,t) = B.breakSubstring search bs

