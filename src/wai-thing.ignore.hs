{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- import qualified Network.Wai.Handler.Warp as Warp

import qualified Network.HTTP.Types.Header as Header
import Network.Wai (Application, Middleware, Request)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Cors as Cors
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import RIO (ByteString)

type BaseUrl = ByteString

setCorsMiddleware :: BaseUrl -> Middleware
setCorsMiddleware baseUrl =
  Cors.cors $
    \request ->
      let allowedOrigins =
            case lookup Header.hOrigin (Wai.requestHeaders request) of
              Just "http://localhost:9051" -> ["http://localhost:9051"]
              _ -> [baseUrl]
       in Just (Cors.simpleCorsResourcePolicy {corsOrigins = Just (allowedOrigins, False)})

-- foo :: Request -> [Char]
foo request = lookup Header.hOrigin (Wai.requestHeaders request)
