{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.OpenAPI
  ( openApiHandler,
    writeOpenApiJSON,
  )
where

--------------------------------------------------------------------------------

import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.OpenApi (OpenApi)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens
import Data.Proxy (Proxy (..))
import Routes.Todo (TodoAPI)
import Servant (Handler)
import Servant.OpenApi qualified as OpenApi

--------------------------------------------------------------------------------

openApiHandler :: Handler OpenApi
openApiHandler = pure todoOpenApi

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

todoOpenApi :: OpenApi
todoOpenApi =
  OpenApi.toOpenApi todoAPI
    & OpenApi.info . OpenApi.title .~ "Todo API"
    & OpenApi.info . OpenApi.version .~ "1.0"
    & OpenApi.info . OpenApi.description ?~ "This is an API that tests OpenAPI integration"
    & OpenApi.info . OpenApi.license ?~ ("MIT" & url ?~ OpenApi.URL "http://mit.com")

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
writeOpenApiJSON :: IO ()
writeOpenApiJSON = BL8.writeFile "./openApi.json" (Aeson.encodePretty todoOpenApi)
