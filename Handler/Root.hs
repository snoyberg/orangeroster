{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root where

import Yesod
import Yesod.Helpers.Auth
import OR
import Settings
import StaticFiles

getRootR :: Handler RepHtml
getRootR = do
    Just fb <- getFacebookUrl AuthR
    defaultLayout $ do
        setTitle "OrangeRoster Homepage"
        addBody $(hamletFile "root")
        addStyle $(cassiusFile "root")
        addJavascript $(juliusFile "root")
