{-# LANGUAGE OverloadedStrings #-}

module Myth.Alert (healthy, getBabyMonitorStatus, getHDHomeRunStatus, getMythTVStatus, getPiholeStatus, getHueStatus) where
import Myth.Common
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Bits.Bitwise hiding (all)
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import Data.Fixed (divMod')
import Data.Maybe (listToMaybe)
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Vector as V
import Control.Exception (try)

healthy = 0 :: Int

getBabyMonitorStatus :: IO (Int)
getBabyMonitorStatus = do
    poem <- getUrl "http://poem:4714/status"
    bard <- getUrl "http://bard:4714/status"
    return . fromListLE $ [(count "state: RUNNING" poem) /= 8, (count "state: RUNNING" bard) /= 2]

parseHDHomeRunStatus = withArray "TunerList" $ do return . (==4) . length

getHDHomeRunStatus = do
    req <- parseRequest "http://hdhomerun/status.json" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return isHealthy
                                        where body = getResponseBody res
                                              isHealthy = case (parse parseHDHomeRunStatus body) of Success result -> result
                                                                                                    Error _        -> False
                           _         -> return False

    return status

parseMythTVStatus = withObject "GetProgramList" $ \o ->
    pure o >>= (.: "ProgramList") >>= (.: "Programs") >>= (mapM (.: "EndTime")) . (V.toList) >>= mapM (return . zonedTimeToUTC) >>= return . listToMaybe

getMythTVStatus = do
    currentUtcTime <- getCurrentTime

    req <- parseRequest "http://192.168.0.65/Guide/GetProgramList?StartTime=2021-01-01T00:00:00&EndTime=2031-01-01T00:00:00&Count=1&Descending=1" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return $ daysInEpg >= 5
                                        where body = getResponseBody res
                                              lastEpgDate    = case (parse parseMythTVStatus body) of Success (Just result) -> result
                                                                                                      _                     -> currentUtcTime
                                              (daysInEpg, _) = timeToDaysAndTimeOfDay (diffUTCTime lastEpgDate currentUtcTime)
                           _         -> return False

    return status

parsePiholeStatus = withObject "Summary" $ \o ->
    pure o >>= (.: "status") >>= return . ((== "enabled") :: String -> Bool)

getPiholeStatus = do
    req <- parseRequest "http://192.168.0.99/admin/api.php" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return isHealthy
                                        where body = getResponseBody res
                                              isHealthy = case (parse parsePiholeStatus body) of Success result -> result
                                                                                                 Error _        -> False
                           _         -> return False

    return status

parseHueStatus = withObject "lights" $ \o ->
    HashMap.traverseWithKey (\_ v -> withObject "light" (\l -> pure l >>= (.: "state") >>= (.: "reachable")) v) o >>= return . (foldr (\c l -> (if c then 0 else 1) + l) 0) . HashMap.elems

getHueStatus :: IO (Int)
getHueStatus = do
    req <- parseRequest "http://philips-hue.lan/api/jJzAag3LO9O49xLU8JK4CmwLzt8T5m7ZdR0rAXus/lights" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return isHealthy
                                        where body = getResponseBody res
                                              isHealthy = case (parse parseHueStatus body) of Success result -> result
                                                                                              Error _        -> (-1)
                           _         -> return (-1) -- Unhealthy

    return status

-- Count the number of substrings in a ByteString
count "" _      = 0
count search bs = if B.null t then 0 else 1 + count search (B.drop (B.length search) t)
    where (h,t) = B.breakSubstring search bs

timeToDaysAndTimeOfDay :: NominalDiffTime -> (Integer, TimeOfDay)
timeToDaysAndTimeOfDay dt =
    let s = realToFrac dt
        (m, ms) = divMod' s 60
        (h, hm) = divMod' m 60
        (d, dh) = divMod' h 24
     in (d, TimeOfDay dh hm ms)

