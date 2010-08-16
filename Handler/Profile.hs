{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile where

import Yesod
import App
import Model
import Settings
import Control.Monad (unless)
import Data.Maybe (isJust)

getProfileR :: UserId -> Handler OR RepHtml
getProfileR srcid = do
    (destid, _) <- reqUserId
    isShared <- fmap isJust $ runDB $ getBy $ UniqueShare srcid destid
    unless isShared $ permissionDenied "The request user is not sharing with you."
    src <- runDB $ get404 srcid
    profile <- runDB $ getMainProfile srcid >>= loadEntry
    applyLayoutW $ addBody $(hamletFile "profile")
