{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module App where

import Control.Monad.IO.Class

import qualified Database.Persist.Sqlite as Sq
import qualified Database.Persist.Sql as Sql

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet

import Web.Scotty

import Model
import Utils

allContest :: IO [(Int, Contest)]
allContest = Sq.runSqlite "db.sqlite" $ do
   Sql.runMigration migrateAll
   contest <- Sq.selectList [] []
   liftIO $ return (zip [1..] $ map Sql.entityVal contest)

app :: IO ()
app = Sq.runSqlite "db.sqlite" $ do
  Sql.runMigration migrateAll

  liftIO $ scotty 36000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"
      >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")

    let userId = "user" :: String

    get "/" $ do
      currentTime <- liftIO getLocalTime :: ActionM String
      contests <- liftIO allContest
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined

    get "admin" $ do
      currentTime <- liftIO getLocalTime
      contests <- liftIO allContest
      html $ renderHtml $ $(hamletFile "./template/admin.hamlet") undefined

    get "setcontest/#" $ do
      currentTime <- liftIO getLocalTime
      contests <- liftIO allContest
      html $ renderHtml $ $(hamletFile "./template/admin.hamlet") undefined

    get "/setcontest/:contestId" $ do
      currentTime <- liftIO getLocalTime
      contestId <- param "contestId" :: ActionM Int
      contests <- liftIO allContest
      let startTime = "not impl -- contestStart contest" :: String
      let endTime = "not impl -- contestEnd contest" :: String
      case lookup contestId contests of
        Nothing -> redirect "../"
        Just contest ->
          if contestSetter contest /= userId
            then redirect "../"
            else html $ renderHtml $ $(hamletFile "./template/setcontest.hamlet") undefined
