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

mkYesodDispatch "OR" resourcesOR

getFaviconR :: Handler OR ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

withOR :: (Application -> IO a) -> IO a
withOR f = Settings.withConnectionPool $ \p -> do
    flip runConnectionPool p $ runMigration $ do
        migrate (undefined :: User)
        migrate (undefined :: FacebookCred)
        migrate (undefined :: Entry)
        migrate (undefined :: EntryData)
        migrate (undefined :: MainProfile)
        migrate (undefined :: Email)
        migrate (undefined :: Share)
    let h = OR s a p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
    a = Auth
            { authIsOpenIdEnabled = False
            , authRpxnowApiKey = Nothing
            , authEmailSettings = Nothing
            , authFacebook = Just (key, secret, ["email"])
            }
    key = "cf58ce060edb6b0442dd0f5b91fade62"
    secret = "00cf78c42797f97b89ae007618f10bf4"
