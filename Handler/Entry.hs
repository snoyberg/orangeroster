{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Entry where

import Yesod
import App
import Model
import Data.Time
import Handler.Home (insertProfile, showProfile)
import Control.Monad (unless)

postEntriesR :: Handler OR ()
postEntriesR = do
    (uid, _) <- reqUserId
    mname <- runFormPost' $ maybeStringInput "name"
    name <- case mname of
                Nothing -> do
                    setMessage "You must provide a name for your entry."
                    redirect RedirectTemporary HomeR
                Just x -> return x
    now <- liftIO getCurrentTime
    eid <- runDB $ do
        pid <- insert $ Profile now
        insert $ Entry uid pid name
    setMessage "Your entry has been created."
    redirect RedirectTemporary $ EntryR eid

getEntryR :: EntryId -> Handler OR RepHtml
getEntryR eid = do
    (uid, _) <- reqUserId
    Entry uid' pid _ <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    profile <- runDB $ loadProfile pid
    let dest = EntryR eid
    hamletToRepHtml $ showProfile profile dest

postEntryR :: EntryId -> Handler OR ()
postEntryR eid = do
    (uid, _) <- reqUserId
    Entry uid' pid _ <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    insertProfile pid
    redirect RedirectTemporary $ EntryR eid
