{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Share where

import Yesod
import App
import Model
import Control.Monad.Trans.Class (lift)
import Data.Time (getCurrentTime)

postShareR :: Handler OR ()
postShareR = do
    (uid, u) <- reqUserId
    (res, _, _) <- runFormPost $ emailInput "email"
    case res of
        FormSuccess email -> do
            runDB $ do
                x <- getBy $ UniqueEmail email
                case x of
                    Just (_, Email dest _ _ _) -> do
                        _ <- insert $ Share uid dest
                        let msg = userDisplayName u ++ " is now sharing with you."
                        now <- liftIO getCurrentTime
                        _ <- insert $ Note dest (string msg) now
                        lift $ setMessage "Sharing initiated"
                    Nothing -> do
                        insertBy $ ShareOffer uid email
                        lift $ setMessage "Sharing offer initiated"
                        -- FIXME send an email invite
        _ -> setMessage "Invalid email address submitted"
    redirect RedirectTemporary HomeR
