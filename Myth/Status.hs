{-# LANGUAGE OverloadedStrings #-}

module Myth.Status where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Aeson.Types
import qualified Data.Vector as V
import Data.Bits.Bitwise
import Control.Exception (try)
import Data.List

parseConnected = withObject "EncoderList" $ \o -> do
    l <- pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "Connected")) . (V.toList)
    return $ (== ("true" :: String)) <$> l

parseActive = withObject "EncoderList" $ \o ->
    pure o >>= (.: "EncoderList") >>= (.: "Encoders") >>= (mapM (.: "State")) . (V.toList) >>= fmap ((> 0) . read)
    --return $ (> 0) . read <$> l

getStatusCode :: IO Int
getStatusCode = do
    req <- parseRequest "http://angel.home:6544/Dvr/GetEncoderList" >>= \req -> return req { requestHeaders = [("Accept", "application/json")] }

    eres <- try $ httpJSON req :: IO (Either HttpException (Response Value))

    let num = case eres of Right res -> fromListLE . concat . transpose $ [connectedEncs, activeEncs]
                                        where [Success connectedEncs, Success activeEncs] = sequence [parse parseConnected, parse parseActive] $ getResponseBody res
                           _         -> 0

    return num

