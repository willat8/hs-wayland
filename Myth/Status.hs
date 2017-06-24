{-# LANGUAGE OverloadedStrings #-}

module Myth.Status where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Bits.Bitwise
import Control.Exception (try)

parseStatus = withObject "EncoderList" $ \o -> do
    statuses <- pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Connected")) . (V.toList)
    return . fromListLE $ (== ("true" :: String)) <$> statuses

getStatusCode :: IO Int
getStatusCode = do
    req <- parseRequest "http://worker1:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")] }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    let num = case eres of Right res -> num where Success num = parse parseStatus $ getResponseBody res
                           _         -> 0

    return num

