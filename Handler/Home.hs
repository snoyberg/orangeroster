{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Yesod
import Yesod.Form.Jquery
import App
import Settings
import Model
import Control.Applicative
import StaticFiles

entryFormlet :: Formlet s m (String, String)
entryFormlet p = fieldsToTable $ (,)
           <$> stringField "Name" (fmap fst p)
           <*> stringField "Value" (fmap snd p)

getHomeR :: Handler OR RepHtml
getHomeR = do
    (uid, u) <- reqUserId
    profile <- runDB $ loadProfile $ userProfile u
    emails <- runDB $ selectList [EmailOwnerEq uid] [] 0 0
    (_, wform, enctype) <- runFormGet $ entryFormlet Nothing
    shares <- runDB $ selectList [ShareDestEq uid] [] 0 0 >>= mapM (\(_, Share srcid _) -> do
        src <- get404 srcid
        return (srcid, src)
        )
    entries <- runDB $ selectList [EntryOwnerEq uid] [EntryTitleAsc] 0 0
    notes <- runDB $ selectList [NoteUserEq uid] [NoteCreationDesc] 10 0 >>= mapM (\(nid, n) -> do
        ls <- selectList [NoteLinkNoteEq nid] [NoteLinkPriorityAsc] 0 0
        return ((nid, n), ls))
    y <- getYesod
    applyLayoutW $ do
        setTitle "Homepage"
        form <- extractBody wform
        addBody $(hamletFile "home")
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addScript $ StaticR jquery_cookie_js
        addStylesheetEither $ urlJqueryUiCss y
        addStyle $(cassiusFile "home")
        addJavascript $(juliusFile "home")

postHomeR :: Handler OR RepHtml
postHomeR = do
    (uid, u) <- reqUserId
    let eid = userProfile u
    (x, wform, enctype) <- runFormPost $ entryFormlet Nothing
    case x of
        FormSuccess (k, v) -> do
            _ <- runDB $ insert $ ProfileData eid k v
            setMessage "Data added to profile"
            redirect RedirectTemporary HomeR
        _ -> return ()
    applyLayoutW $ do
        setTitle "Add to profile"
        form <- extractBody wform
        addBody [$hamlet|
%form!method=post!enctype=$enctype$
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit!value=Add
|]

postDisplayNameR :: Handler OR ()
postDisplayNameR = do
    (uid, _) <- reqUserId
    (res, _, _) <- runFormPost $ stringInput "display-name"
    case res of
        FormSuccess dn -> do
            runDB $ update uid [UserDisplayName dn]
            setMessage $ string $ "Display name changed to " ++ dn
        _ -> setMessage "There was an error in your submission"
    redirect RedirectTemporary HomeR
