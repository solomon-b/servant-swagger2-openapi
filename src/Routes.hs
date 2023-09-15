{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

module Routes
  ( module OpenAPI,
    module Swagger,
    module Todo,
    API,
    server,
  )
where

--------------------------------------------------------------------------------

import Data.OpenApi (OpenApi)
import Data.Swagger
import Routes.Swagger as Swagger
import Routes.OpenAPI as OpenAPI
import Routes.Todo as Todo
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.API qualified as API
import Servant.API.Verbs qualified as Verb

--------------------------------------------------------------------------------

-- | Combined server of a Todo service with Swagger documentation.
server :: Servant.Server API
server = swaggerHandler :<|> openApiHandler :<|> todoHandler

-- | Combined API of a Todo service with Swagger documentation.
type API = SwaggerAPI :<|> OpenAPI :<|> TodoAPI

type SwaggerAPI = "swagger.json" :> Verb.Get '[API.JSON] Swagger

type OpenAPI = "openApi.json" :> Verb.Get '[API.JSON] OpenApi
