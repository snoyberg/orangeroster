{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Controller
    ( withOR
    ) where

import Data.ByteString.Char8
import Database.Persist.GenericSql
import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static

import Handler.Delete
import Handler.Entry
import Handler.Home
import Handler.Profile
import Handler.Note
import Handler.Root
import Handler.Share
import OR
import Model
import Settings

mkYesodDispatch "OR" resourcesOR

getFaviconR :: Handler ()
getFaviconR = sendFile (pack "image/x-icon") "favicon.ico"

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
    s = static Settings.staticdir
