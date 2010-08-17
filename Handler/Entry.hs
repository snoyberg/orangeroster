{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Entry where

import Yesod
import App
import Settings
import Model
import Control.Applicative
import Data.Time
import Handler.Home (entryFormlet)
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
    Entry uid' pid name <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    profile <- runDB $ loadProfile pid
    (_, wform, enctype) <- runFormGet $ entryFormlet Nothing
    applyLayoutW $ do
        setTitle $ string name
        form <- extractBody wform
        addBody $(hamletFile "entry")

postEntryR :: EntryId -> Handler OR RepHtml
postEntryR eid = do
    (uid, _) <- reqUserId
    Entry uid' pid name <- runDB $ get404 eid
    unless (uid == uid') $ permissionDenied "You do not own that entry."
    (res, wform, enctype) <- runFormPost $ entryFormlet Nothing
    case res of
        FormSuccess (k, v) -> do
            _ <- runDB $ insert $ ProfileData pid k v
            setMessage "Data added to entry"
            redirect RedirectTemporary $ EntryR eid
        _ -> return ()
    applyLayoutW $ do
        setTitle $ string name
        form <- extractBody wform
        addBody [$hamlet|
%h1 $name$
%form!method=post!enctype=$enctype$
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit
|]
