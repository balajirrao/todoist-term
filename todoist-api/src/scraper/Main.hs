{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (Alternative (many))
import Data.Default ( Default(def) )
import Data.Maybe (fromJust)
import Data.Yaml ( encodeFile, ToJSON )
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Text.HTML.Scalpel
    ( scrapeURLWithConfig,
      attr,
      chroot,
      chroots,
      html,
      text,
      (@:),
      (@=),
      hasClass,
      inSerial,
      seekNext,
      Config(manager),
      Scraper )

import Todoist.API.Sync

managerSettings :: HTTP.ManagerSettings
managerSettings =
  HTTP.tlsManagerSettings
    { HTTP.managerModifyRequest = \req -> do
        req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
        return $
          req'
            { HTTP.requestHeaders =
                (HTTP.hUserAgent, "haskell-scalpel") :
                HTTP.requestHeaders req'
            }
    }

scrapeTable :: Scraper String [Property]
scrapeTable = chroots "tr" $
  inSerial $ do
    textTd <- seekNext $ text "td"
    description <- seekNext $ text "td"

    let property = (head $ words textTd)
        datatype = (unwords $ tail $ words textTd)
    pure $ Property {..}

scraper :: Scraper String [ResourceType]
scraper = chroot ("div" @: [hasClass "content"]) $
  inSerial $ do
    projects <- resourceTypeScraper $ "h1" @: ["id" @= "projects"]
    rest <- many $ resourceTypeScraper "h1"
    pure $ projects : rest
  where
    resourceTypeScraper h1Selector = do
      name <- seekNext $ attr "id" $ h1Selector
      _ <- seekNext $ html $ "h3" @: ["id" @= "properties"]
      ResourceType name <$> (seekNext $ chroot "tbody" $ scrapeTable)

main :: IO ()
main = do
  manager <- Just <$> HTTP.newManager managerSettings
  result <- scrapeURLWithConfig (def {manager}) url $ scraper
  maybe printError printJson result
  where
    url = "https://developer.todoist.com/sync/v8/"
    printError = putStrLn "Failed"
    printJson = encodeFile "todoist-sync-api-v8.yml"