{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where

import Yesod
import App

getORLogoutR :: Handler OR ()
getORLogoutR = clearUserId >> redirect RedirectTemporary RootR
