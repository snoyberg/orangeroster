{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Entry where

import Yesod
import Yesod.Helpers.Auth
import OR
import Model
import Data.Time
import Handler.Home (insertProfile, showProfile)
import Control.Monad (unless)
import Settings

postEntriesR :: Handler ()
postEntriesR = do
    uid <- requireAuthId
    mname <- runFormPost' $ maybeStringInput "name"
    name <- case mname of
                Nothing -> do
                    setMessage "You must provide a name for your entry."
                    redirect RedirectTemporary HomeR
                Just x -> return x
    now <- liftIO getCurrentTime
    _ <- runDB $ do
        pid <- insert $ Profile now
        insert $ Entry uid pid name
    setMessage "Your entry has been created."
    redirect RedirectTemporary HomeR

getEntryR :: EntryId -> Handler RepHtml
getEntryR eid = do
    uid <- requireAuthId
    Entry uid' pid name <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    profile <- runDB $ loadProfile pid
    let showProfile' = showProfile profile $ Just $ EntryR eid
    hamletToRepHtml $(hamletFile "entry")

postEntryR :: EntryId -> Handler ()
postEntryR eid = do
    uid <- requireAuthId
    Entry uid' pid _ <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    insertProfile pid
    redirect RedirectTemporary HomeR

postEntryNameR :: EntryId -> Handler ()
postEntryNameR eid = do
    uid <- requireAuthId
    Entry uid' _ _ <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    (res, _, _) <- runFormPost $ stringInput "name"
    case res of
        FormSuccess name -> do
            runDB $ update eid [EntryTitle name]
            setMessage "Entry name changed"
        _ -> setMessage "Invalid name submitted"
    redirect RedirectTemporary HomeR

postDeleteEntryR :: EntryId -> Handler ()
postDeleteEntryR eid = do
    uid <- requireAuthId
    Entry uid' _ _ <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    runDB $ delete eid
    setMessage "Entry deleted"
    redirect RedirectTemporary HomeR
