{-# LANGUAGE TemplateHaskell #-}

module Todoist.API.Internal.Sync where

import Data.ByteString
import Data.FileEmbed

apiSpecYaml :: ByteString
apiSpecYaml = $(embedFile "todoist-sync-api-v8.yml")