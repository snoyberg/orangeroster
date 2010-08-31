{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile where

import Yesod
import Yesod.Helpers.Auth
import OR
import Model
import Settings
import Control.Monad (unless)
import Data.Maybe (isJust)
import Handler.Home (showProfile)

getProfileR :: UserId -> Handler RepHtml
getProfileR srcid = do
    destid <- requireAuthId
    isShared <- fmap isJust $ runDB $ getBy $ UniqueShare srcid destid
    unless isShared $ permissionDenied "The request user is not sharing with you."
    src <- runDB $ get404 srcid
    profile <- runDB $ loadProfile $ userProfile src
    let showProfile' = showProfile profile Nothing
    hamletToRepHtml $(hamletFile "profile")
