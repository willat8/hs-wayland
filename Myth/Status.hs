{-# LANGUAGE OverloadedStrings #-}

module Myth.Status where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson (Value)
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Bits.Bitwise

parseEncoder :: Value -> Parser String
parseEncoder = withObject "Encoder" $ \o -> do
    enc <- pure o >>= (.: "Connected")
    return enc

parseEncoders :: Value -> Parser Int
parseEncoders = withObject "EncoderList" $ \o -> do
    encs <- pure o >>= (.: "EncoderList") >>= (.: "Encoders")
    let Success statuses = parse (withArray "Encoders" $ \a -> mapM parseEncoder (V.toList a)) encs
    return . fromListLE $ (== "true") <$> statuses

getStatusCode = do
    req <- parseRequest "http://worker1:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")] }

    res <- getResponseBody <$> httpJSON req :: IO Value
    
    let Success num = parse parseEncoders res

    return num

