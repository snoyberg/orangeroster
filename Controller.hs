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
import Handler.Note

mkYesodDispatch "OR" resourcesOR

getFaviconR :: Handler OR ()
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
    let h = OR s (a p) p
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
    a p = Auth
            { authIsOpenIdEnabled = False
            , authRpxnowApiKey = Nothing
            , authEmailSettings = Just $ emailSettings p
            , authFacebook = Just (facebookKey, facebookSecret, ["email"])
            }

emailSettings :: ConnectionPool -> AuthEmailSettings
emailSettings p = AuthEmailSettings
    { addUnverified = \email verkey -> flip runConnectionPool p $ do
        uid <- newUser email
        fmap fromIntegral $ insert $ Email uid email (Just verkey) False
    , sendVerifyEmail = \email verkey verurl -> do
        print ("FIXME sendVerifyEmail", email, verkey, verurl)
    , getVerifyKey = \emailid -> flip runConnectionPool p $ do
        x <- get $ fromIntegral emailid
        return $ maybe Nothing emailVerkey x
    , setVerifyKey = \emailid verkey -> flip runConnectionPool p $
        update (fromIntegral emailid) [EmailVerkey $ Just verkey]
    , verifyAccount = \emailid -> flip runConnectionPool p $
        update (fromIntegral emailid) [EmailVerified True]
    , setPassword = \emailid' password -> flip runConnectionPool p $ do
        let emailid = fromIntegral emailid'
        x <- get emailid
        case x of
            Nothing -> return ()
            Just (Email uid _ _ _) -> do
                update uid [UserPassword $ Just password]
                update emailid [EmailVerkey Nothing]
    , getEmailCreds = \email -> flip runConnectionPool p $ do
        x <- getBy $ UniqueEmail email
        case x of
            Nothing -> return Nothing
            Just (eid, e) -> do
                mu <- get $ emailOwner e
                let pass = maybe Nothing userPassword mu
                return $ Just EmailCreds
                    { emailCredsId = fromIntegral eid
                    , emailCredsPass = pass
                    , emailCredsStatus = emailVerified e
                    , emailCredsVerkey = emailVerkey e
                    }
    , getEmail = \emailid -> flip runConnectionPool p $ do
        -- FIXME :: EmailId -> IO (Maybe Email)
        x <- get $ fromIntegral emailid
        return $ fmap emailEmail x
    }
