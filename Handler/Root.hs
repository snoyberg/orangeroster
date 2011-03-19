{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Root where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.Facebook
import OR
import Settings
import StaticFiles

getRootR :: Handler RepHtml
getRootR = do
    let emailLogin = AuthR $ PluginR "email" ["login"]
        emailRegister = AuthR $ PluginR "email" ["register"]
    defaultLayout $ do
        setTitle "OrangeRoster Homepage"
        addHamlet $(hamletFile "root")
        addCassius $(cassiusFile "root")
        addJulius $(juliusFile "root")
