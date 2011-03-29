{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Delete where

import Text.Blaze
import Yesod
import Yesod.Helpers.Auth

import Model
import OR

deleteHelper :: PersistEntity x
             => String -> (x -> ProfileId) -> (Key x) -> Handler ()
deleteHelper s f did = do
    uid <- requireAuthId
    d <- runDB $ get404 did
    let pid = f d
    _isProfile <- runDB $ do
        mu <- getBy $ UniqueUserProfile pid
        case mu of
            Nothing -> do
                mx <- getBy $ UniqueEntryProfile pid
                case mx of
                    Nothing -> lift notFound
                    Just (_, Entry owner _ _) ->
                        if owner == uid
                            then return False
                            else lift notFound
            Just (uid', _) ->
                if uid == uid'
                    then return True
                    else lift notFound
    runDB $ delete did
    setMessage $ string $ s ++ " deleted"
    redirect RedirectTemporary HomeR

postDeletePhoneR :: PhoneId -> Handler ()
postDeletePhoneR = deleteHelper "Phone number" phoneProfile

postDeleteAddressR :: AddressId -> Handler ()
postDeleteAddressR = deleteHelper "Address" addressProfile

postDeleteScreenNameR :: ScreenNameId -> Handler ()
postDeleteScreenNameR = deleteHelper "Screen name" screenNameProfile

postDeleteMiscR :: MiscId -> Handler ()
postDeleteMiscR = deleteHelper "Misc" miscProfile
