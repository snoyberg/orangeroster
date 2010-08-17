{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Share where

import Yesod
import App
import Model
import Control.Monad.Trans.Class (lift)

postShareR :: Handler OR ()
postShareR = do
    (uid, _) <- reqUserId
    (res, _, _) <- runFormPost $ emailInput "email"
    case res of
        FormSuccess email -> do
            runDB $ do
                x <- getBy $ UniqueEmail email
                case x of
                    Just (_, Email dest _ _ _) -> do
                        _ <- insert $ Share uid dest
                        lift $ setMessage "Sharing initiated"
                    Nothing -> do
                        insertBy $ ShareOffer uid email
                        lift $ setMessage "Sharing offer initiated"
                        -- FIXME send an email invite
        _ -> setMessage "Invalid email address submitted"
    redirect RedirectTemporary HomeR
