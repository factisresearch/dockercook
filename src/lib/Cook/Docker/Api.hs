module Cook.Docker.Api where

import Cook.Docker.Types

import Network.Wreq
import Control.Lens

data DockerApi
   = DockerApi
   { dapi_endpointUrl :: String
   } deriving (Show, Eq)

dockerImages :: DockerApi -> IO [DockerImage]
dockerImages dapi =
    do r <- asJSON =<< get (dapi_endpointUrl dapi ++ "/images/json?all=0")
       case r ^? responseBody of
         Nothing ->
             fail "Docker request failed."
         Just img ->
             return img
