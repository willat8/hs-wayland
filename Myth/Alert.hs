{-# LANGUAGE OverloadedStrings #-}

module Myth.Alert (getBabyMonitorStatus) where
import Myth.Common
import qualified Data.ByteString as B

getBabyMonitorStatus = do
    s <- getUrl "http://poem:4714/status"
    return ((count "state: RUNNING" s) == 8)

-- Count the number of substrings in a ByteString
count "" _      = 0
count search bs = if B.null t then 0 else 1 + count search (B.drop (B.length search) t)
    where (h,t) = B.breakSubstring search bs

