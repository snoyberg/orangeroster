{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Root where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.Facebook
import OR
import Settings
import StaticFiles

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- OR.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    let emailLogin = AuthR $ PluginR "email" ["login"]
        emailRegister = AuthR $ PluginR "email" ["register"]
    defaultLayout $ do
        setTitle "OrangeRoster Homepage"
        addWidget $(widgetFile "root")
