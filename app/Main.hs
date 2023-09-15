{-# LANGUAGE ImportQualifiedPost #-}

module Main where

--------------------------------------------------------------------------------

import Routes
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp
import Servant qualified

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Generating Swagger File"
  writeSwaggerJSON
  putStrLn "Generating OpenApi File"
  writeOpenApiJSON
  putStrLn "Running on port 8000"
  run 8000 $ Servant.serve (Proxy :: Proxy API) server
