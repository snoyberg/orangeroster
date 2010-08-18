{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Note where

import Yesod
import Yesod.Form.Jquery
import App
import Settings
import Model
import Control.Applicative

postNoteCloseR :: NoteId -> Handler OR ()
postNoteCloseR nid = do
    runDB $ do
        deleteWhere [NoteLinkNoteEq nid]
        delete nid
    setMessage "Note closed"
    redirect RedirectTemporary HomeR
