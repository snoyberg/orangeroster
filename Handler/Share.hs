{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Share where

import Yesod
import App
import Model
import Data.Time (getCurrentTime)

--startShare :: UserId -> UserId -> Handle
startShare :: UserId -> User -> UserId -> Handler OR ()
startShare uid u dest = do
    runDB $ do
        _ <- insert $ Share uid dest
        let msg = userDisplayName u ++ " is now sharing with you."
        now <- liftIO getCurrentTime
        _ <- insert $ Note dest (string msg) now
        return ()
    setMessage "Sharing initiated"

postShareR :: Handler OR ()
postShareR = do
    (uid, u) <- reqUserId
    (res, _, _) <- runFormPost $ emailInput "email"
    case res of
        FormSuccess email -> do
            x <- runDB $ getBy $ UniqueEmail email
            case x of
                Just (_, Email (Just dest) _ _) -> startShare uid u dest
                _ -> runDB $ do
                    _ <- insertBy $ ShareOffer uid email
                    lift $ setMessage "Sharing offer initiated"
                    -- FIXME send an email invite
        _ -> setMessage "Invalid email address submitted"
    redirect RedirectTemporary HomeR

postShareUserR :: UserId -> Handler OR ()
postShareUserR dest = do
    (uid, u) <- reqUserId
    startShare uid u dest
    redirect RedirectTemporary HomeR

postStopShareUserR :: UserId -> Handler OR ()
postStopShareUserR dest = do
    (uid, _) <- reqUserId
    runDB $ deleteBy $ UniqueShare uid dest
    setMessage "No longer sharing"
    redirect RedirectTemporary HomeR
