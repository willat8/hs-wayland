{-# LANGUAGE OverloadedStrings #-}

module Myth.Common (getUrl) where
import Network.HTTP.Simple
import Network.HTTP.Client
import Control.Exception (try)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B

getUrl url = do
    bsreq <- parseRequest url >>= \req -> return req { responseTimeout = Just 5000000 }
    ebs <- try $ B.toStrict . getResponseBody <$> httpLBS bsreq :: IO (Either HttpException S.ByteString)
    return $ case ebs of Right bs -> bs
                         _        -> S.empty

