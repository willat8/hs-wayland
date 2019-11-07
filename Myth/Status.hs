{-# LANGUAGE OverloadedStrings #-}

module Myth.Status (getEncodersStatus) where
import Myth.Internal
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import qualified Data.Vector as V
import Control.Exception (try)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.List (zipWith4)

parseConnected = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Connected")) . (V.toList) >>= return . fmap (== ("true" :: String))

parseActive = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "State")) . (V.toList) >>= return . fmap ((> 0) . read)

parseTitles = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Recording")) . (V.toList) >>= mapM (.: "Title")

parseIconPaths = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Recording")) . (V.toList) >>= mapM (.: "Channel") >>= mapM (.: "IconURL")

getEncodersStatus = do
    req <- parseRequest "http://192.168.0.10:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 5000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    status <- case eres of Right res -> zipWith4 Encoder connectedEncs activeEncs recordingTitles <$> channelIcons
                                        where body = getResponseBody res
                                              Success connectedEncs    = parse parseConnected body
                                              Success activeEncs       = parse parseActive body
                                              Success recordingTitles  = parse parseTitles body
                                              Success channelIconPaths = parse parseIconPaths body
                                              channelIcons = mapM getChannelIcon channelIconPaths
                           _         -> return []

    return status

getChannelIcon "" = return S.empty
getChannelIcon path = do
    bsreq <- parseRequest ("http://192.168.0.10:6544" ++ path) >>= \req -> return req { responseTimeout = Just 5000000 }
    ebs <- try $ B.toStrict . getResponseBody <$> httpLBS bsreq :: IO (Either HttpException S.ByteString)
    return $ case ebs of Right bs -> bs
                         _        -> S.empty

