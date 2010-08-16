{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root where

import Yesod
import Yesod.Helpers.Auth
import App
import Settings

getRootR :: Handler OR RepHtml
getRootR = do
    applyLayoutW $ do
        addBody $(hamletFile "root")
