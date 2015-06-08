{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Model where

import Data.Time

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Contest
  name String
  start UTCTime
  end UTCTime
  setter String
Problem
  contest String
  problem String
  input String
  output String
Status
  time UTCTime
  user String
  contest Int
  problem String
  judge Bool
  deriving Show
User
  name String
  team String
|]
