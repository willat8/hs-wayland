{-# LANGUAGE OverloadedStrings #-}

module Myth.Alert (healthy, getBabyMonitorStatus, getHDHomeRunStatus, getMythTVStatus, getPiholeStatus, getHueStatus) where
import Myth.Common
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Bits.Bitwise
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
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
                                              Success isHealthy = parse parseHDHomeRunStatus body
                           _         -> return False

    return status

parseMythTVStatus = withObject "SettingList" $ \o ->
    pure o >>= (.: "Settings") >>= (.: "mythfilldatabaseLastRunStatus") >>= return . ((== "Successful.") :: String -> Bool)

getMythTVStatus = do
    diskstatus <- ((== "all pools are healthy") . trim) <$> getUrl "http://rancher/"
    req <- parseRequest "http://rancher:6544/Myth/GetSettingList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return isHealthy
                                        where body = getResponseBody res
                                              Success isHealthy = parse parseMythTVStatus body
                           _         -> return False

    return (diskstatus && status)

parsePiholeStatus = withObject "Summary" $ \o ->
    pure o >>= (.: "status") >>= return . ((== "enabled") :: String -> Bool)

getPiholeStatus = do
    req <- parseRequest "http://pi.hole/admin/api.php" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return isHealthy
                                        where body = getResponseBody res
                                              Success isHealthy = parse parsePiholeStatus body
                           _         -> return False

    return status

parseHueStatus = withObject "lights" $ \o ->
    pure o >>= return . (== 6) . length . HashMap.toList

getHueStatus = do
    req <- parseRequest "http://philips-hue.lan/api/jJzAag3LO9O49xLU8JK4CmwLzt8T5m7ZdR0rAXus/lights" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> return isHealthy
                                        where body = getResponseBody res
                                              Success isHealthy = parse parseHueStatus body
                           _         -> return False

    return status

-- Count the number of substrings in a ByteString
count "" _      = 0
count search bs = if B.null t then 0 else 1 + count search (B.drop (B.length search) t)
    where (h,t) = B.breakSubstring search bs

trim = C.reverse . C.dropWhile isSpace . C.reverse

