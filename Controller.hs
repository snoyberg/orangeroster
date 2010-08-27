{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withOR
    ) where

import OR
import Model
import Settings
import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static
import Database.Persist.GenericSql

import Handler.Root
import Handler.Home
import Handler.Share
import Handler.Profile
import Handler.Entry
import Handler.Note

mkYesodDispatch "OR" resourcesOR

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

withOR :: (Application -> IO a) -> IO a
withOR f = Settings.withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration $ do
        migrate (undefined :: Profile)
        migrate (undefined :: Phone)
        migrate (undefined :: Address)
        migrate (undefined :: ScreenName)
        migrate (undefined :: Misc)

        migrate (undefined :: User)
        migrate (undefined :: FacebookCred)
        migrate (undefined :: Email)
        migrate (undefined :: Share)
        migrate (undefined :: ShareOffer)
        migrate (undefined :: Entry)
        migrate (undefined :: Note)
        migrate (undefined :: NoteLink)
    let h = OR s p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
