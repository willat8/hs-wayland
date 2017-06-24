{-# LANGUAGE OverloadedStrings #-}

module Myth.Status where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Bits.Bitwise

parseStatus = withObject "EncoderList" $ \o -> do
    statuses <- pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Connected")) . (V.toList)
    return . fromListLE $ (== ("true" :: String)) <$> statuses

getStatusCode :: IO Int
getStatusCode = do
    req <- parseRequest "http://worker1:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")] }

    res <- getResponseBody <$> httpJSON req

    let Success num = parse parseStatus res

    return num

