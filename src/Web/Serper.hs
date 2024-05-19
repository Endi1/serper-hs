{-# LANGUAGE OverloadedStrings #-}

module Web.Serper
  ( search,
  )
where

import Data.Aeson (decode, encode, object, (.=))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO.ByteString
import Web.Serper.Types (SerperResult)

search :: ByteString -> String -> IO (Maybe SerperResult)
search apiKey query = do
  manager <- newManager tlsManagerSettings
  let requestObject = object ["q" .= query]
  initialRequest <- parseRequest "https://google.serper.dev/search"
  let request = initialRequest {method = "POST", requestBody = RequestBodyLBS $ encode requestObject, requestHeaders = [("X-API-KEY", apiKey), ("Content-Type", "application/json")]}
  response <- httpLbs request manager
  print response
  return $ decode $ responseBody response
