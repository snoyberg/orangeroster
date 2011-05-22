{-# LANGUAGE OverloadedStrings #-}
module Handler.Note where

import Yesod
import OR
import Model

postNoteCloseR :: NoteId -> Handler ()
postNoteCloseR nid = do
    runDB $ do
        deleteWhere [NoteLinkNoteEq nid]
        delete nid
    setMessage "Note closed"
    redirect RedirectTemporary HomeR
