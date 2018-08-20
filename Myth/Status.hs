{-# LANGUAGE OverloadedStrings #-}

module Myth.Status where
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

getEncodersStatus :: IO [Encoder]
getEncodersStatus = do
    req <- parseRequest "http://angel.home:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")], responseTimeout = Just 1000000 }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    let status = case eres of Right res -> zipWith Encoder connectedEncs activeEncs
                                           where [Success connectedEncs, Success activeEncs] = sequence [parse parseConnected, parse parseActive] $ getResponseBody res
                              _         -> []

    return status

