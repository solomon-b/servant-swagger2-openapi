{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Swagger
  ( swaggerHandler,
    writeSwaggerJSON,
  )
where

--------------------------------------------------------------------------------

import Control.Lens ((&), (.~), (?~))
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Proxy (Proxy (..))
import Data.Swagger (Swagger)
import Data.Swagger qualified as Swagger
import Routes.Todo (TodoAPI)
import Servant (Handler)
import Servant.Swagger (HasSwagger (..))

--------------------------------------------------------------------------------

swaggerHandler :: Handler Swagger
swaggerHandler = pure todoSwagger

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

todoSwagger :: Swagger
todoSwagger =
  toSwagger todoAPI
    & Swagger.info . Swagger.title .~ "Todo API"
    & Swagger.info . Swagger.version .~ "1.0"
    & Swagger.info . Swagger.description ?~ "This is an API that tests swagger integration"
    & Swagger.info . Swagger.license ?~ ("MIT" & Swagger.url ?~ Swagger.URL "http://mit.com")

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "./swagger.json" (Aeson.encodePretty todoSwagger)
