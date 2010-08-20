{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root where

import Yesod
import Yesod.Helpers.Auth
import App
import Settings
import StaticFiles

getRootR :: Handler OR RepHtml
getRootR = do
    applyLayoutW $ do
        setTitle "OrangeRoster Homepage"
        addBody $(hamletFile "root")
        addStyle $(cassiusFile "root")
        addJavascript $(juliusFile "root")
