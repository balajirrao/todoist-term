{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Todoist.API.Sync where

import Data.Yaml ( ToJSON )
   
import GHC.Generics (Generic)
data Property = Property
  { property :: String,
    datatype :: String,
    description :: String
  }
  deriving (Show, Generic, ToJSON)

data ResourceType = ResourceType
  { name :: String,
    properties :: [Property]
  }
  deriving (Show, Generic, ToJSON)
