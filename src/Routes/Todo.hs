{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Todo
  ( todoHandler,
    Todo (..),
    TodoId (..),
    TodoAPI,
  )
where

--------------------------------------------------------------------------------

import Control.Lens ((&), (?~))
import Control.Lens.Combinators (mapped)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Data (Typeable)
import Data.OpenApi qualified as OpenApi
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time qualified as Time
import GHC.Generics (Generic)
import Servant (FromHttpApiData, (:<|>) (..), (:>))
import Servant.API qualified as API
import Servant.API.Verbs qualified as Verb
import Servant.Server qualified as Servant

--------------------------------------------------------------------------------
-- Routes

-- | The API of a Todo service.
type TodoAPI =
  "todo" :> Verb.Get '[API.JSON] [Todo]
    :<|> "todo" :> API.ReqBody '[API.JSON] Todo :> API.Post '[API.JSON] TodoId
    :<|> "todo" :> API.Capture "id" TodoId :> Verb.Get '[API.JSON] Todo
    :<|> "todo" :> API.Capture "id" TodoId :> API.ReqBody '[API.JSON] Todo :> Verb.Put '[API.JSON] TodoId

--------------------------------------------------------------------------------
-- Handlers

todoHandler :: Servant.Server TodoAPI
todoHandler = getTodosHandler :<|> undefined :<|> getTodo :<|> undefined

getTodo :: TodoId -> Servant.Handler Todo
getTodo (TodoId 0) = pure $ Todo (Time.UTCTime (Time.ModifiedJulianDay 1) 1000) "Go to bed"
getTodo (TodoId 1) = pure $ Todo (Time.UTCTime (Time.ModifiedJulianDay 1) 1000) "Finish this example"
getTodo _ = fail "No such todo"

getTodosHandler :: Servant.Handler [Todo]
getTodosHandler =
  pure
    [ Todo (Time.UTCTime (Time.ModifiedJulianDay 1) 1000) "Finish this example",
      Todo (Time.UTCTime (Time.ModifiedJulianDay 1) 1000) "Go to bed"
    ]

--------------------------------------------------------------------------------

-- | A single Todo entry.
data Todo = Todo
  { -- | Creation datetime.
    created :: UTCTime,
    -- | Task summary.
    summary :: Text
  }
  deriving (Show, Generic, Typeable)

instance ToJSON Todo

instance FromJSON Todo

instance Swagger.ToSchema Todo where
  declareNamedSchema proxy =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions proxy
      & mapped . Swagger.schema . Swagger.description ?~ "A Todo list item"
      & mapped . Swagger.schema . Swagger.example ?~ Aeson.toJSON (Todo (Time.UTCTime (Time.fromGregorian 2015 12 31) 0) "get milk")

instance OpenApi.ToSchema Todo where
  declareNamedSchema proxy =
    OpenApi.genericDeclareNamedSchema OpenApi.defaultSchemaOptions proxy
      & mapped . OpenApi.schema . OpenApi.description ?~ "A Todo list item"
      & mapped . OpenApi.schema . OpenApi.example ?~ Aeson.toJSON (Todo (Time.UTCTime (Time.fromGregorian 2015 12 31) 0) "get milk")

-- | A unique Todo entry ID.
newtype TodoId = TodoId Int
  deriving stock (Generic)
  deriving newtype (Show, Typeable, ToJSON, FromHttpApiData)

instance Swagger.ToParamSchema TodoId

instance OpenApi.ToParamSchema TodoId

instance Swagger.ToSchema TodoId

instance OpenApi.ToSchema TodoId
