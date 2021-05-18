{-# LANGUAGE OverloadedStrings #-}

module Myth.Alert (healthy, getBabyMonitorStatus, getHDHomeRunStatus) where
import Myth.Common
import qualified Data.ByteString as B
import Data.Bits.Bitwise
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import qualified Data.Vector as V
import Control.Exception (try)

healthy :: Int
healthy = 0

getBabyMonitorStatus :: IO (Int)
getBabyMonitorStatus = do
    poem <- getUrl "http://poem:4714/status"
    bard <- getUrl "http://bard:4714/status"
    return . fromListLE $ [(count "state: RUNNING" poem) /= 8, (count "state: RUNNING" bard) /= 2]

parseHDHomeRunStatus = withArray "TunerList" $ \a ->
    pure a >>= return . length . V.toList

getHDHomeRunStatus :: IO (Int)
getHDHomeRunStatus = do
    req <- parseRequest "http://hdhomerun/status.json" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return tunerCount
                                        where body = getResponseBody res
                                              Success tunerCount = parse parseHDHomeRunStatus body
                           _         -> return 0

    return status

-- Count the number of substrings in a ByteString
count "" _      = 0
count search bs = if B.null t then 0 else 1 + count search (B.drop (B.length search) t)
    where (h,t) = B.breakSubstring search bs

