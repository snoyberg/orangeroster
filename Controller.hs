{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withOR
    ) where

import App
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

mkYesodDispatch "OR" resourcesOR

getFaviconR :: Handler OR ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

withOR :: (Application -> IO a) -> IO a
withOR f = Settings.withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration $ do
        migrate (undefined :: Profile)
        migrate (undefined :: ProfileData)
        migrate (undefined :: User)
        migrate (undefined :: FacebookCred)
        migrate (undefined :: Email)
        migrate (undefined :: Share)
        migrate (undefined :: Entry)
    let h = OR s a p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
    a = Auth
            { authIsOpenIdEnabled = False
            , authRpxnowApiKey = Nothing
            , authEmailSettings = Nothing
            , authFacebook = Just (facebookKey, facebookSecret, ["email"])
            }
