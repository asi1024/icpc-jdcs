{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

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

main :: IO ()
main = do
  Sq.runSqlite "db.sqlite" $ Sq.runMigration migrateAll
  scotty 36384 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"
      >-> (contains "/js/" <|> contains "/css/" <|> contains "/image/")

    let user_id = "user" :: String

    get "/" $ do
      currentTime <- liftIO getLocalTime
      contests_ <- liftIO (Sq.runSqlite "db.sqlite" (Sq.selectList [] []))
      let contests = zip [1..] $ map Sql.entityVal contests_ :: [(Int, Contest)]
      html $ renderHtml $ $(hamletFile "./template/index.hamlet") undefined
