{-# LANGUAGE OverloadedStrings #-}

module Myth.Status (getEncodersStatus) where
import Myth.Internal
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import qualified Data.Vector as V
import Control.Exception (try)

parseConnected = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Connected")) . (V.toList) >>= return . fmap (== ("true" :: String))

parseActive = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "State")) . (V.toList) >>= return . fmap ((> 0) . read)

parseTitles = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Recording")) . (V.toList) >>= mapM (.: "Title")

getEncodersStatus = do
    req <- parseRequest "http://angel:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 1000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    let status = case eres of Right res -> zipWith3 Encoder connectedEncs activeEncs recordingTitles
                                           where body = getResponseBody res
                                                 Success connectedEncs    = parse parseConnected body
                                                 Success activeEncs       = parse parseActive body
                                                 Success recordingTitles  = parse parseTitles body
                              _         -> []

    return status

