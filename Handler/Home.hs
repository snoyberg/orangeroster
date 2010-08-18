{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Yesod
import Yesod.Form.Core
import Yesod.Form.Jquery
import App
import Settings
import Model
import Control.Applicative
import StaticFiles

entryForm :: FormInput s m (PType, String, String)
entryForm = pure (,,)
            <*> ptypeForm "type"
            <*> stringInput "name"
            <*> stringInput "value"

ptypeForm :: String -> FormInput s m PType
ptypeForm name = GForm $ do
    env <- askParams
    let res = case lookup name env of
                Nothing -> FormMissing
                Just "phone" -> FormSuccess PTPhone
                Just "address" -> FormSuccess PTAddress
                Just "screen-name" -> FormSuccess PTScreenName
                Just "misc" -> FormSuccess PTMisc
                Just x -> FormFailure ["Invalid PType: " ++ x]
    return (res, mempty, UrlEncoded)

getHomeR :: Handler OR RepHtml
getHomeR = do
    (uid, u) <- reqUserId
    profile <- runDB $ loadProfile $ userProfile u
    emails <- runDB $ selectList [EmailOwnerEq uid] [] 0 0
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
        let showProfile' = showProfile profile HomeR
        addBody $(hamletFile "home")
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addScript $ StaticR jquery_cookie_js
        addStylesheetEither $ urlJqueryUiCss y
        addStyle $(cassiusFile "home")
        addJavascript $(juliusFile "home")

data PType = PTPhone | PTAddress | PTScreenName | PTMisc

postHomeR :: Handler OR ()
postHomeR = do
    (_, u) <- reqUserId
    insertProfile $ userProfile u
    redirect RedirectTemporary HomeR

insertProfile :: ProfileId -> Handler OR ()
insertProfile pid = do
    (x, _, _) <- runFormPost entryForm
    case x of
        FormSuccess (pt, k, v) -> do
            case pt of
                PTPhone -> runDB $ insert (Phone pid k v) >> return ()
                PTAddress -> runDB $ insert (Address pid k $ Textarea v) >> return ()
                PTScreenName -> runDB $ insert (ScreenName pid k v) >> return ()
                PTMisc -> runDB $ insert (Misc pid k $ Textarea v) >> return ()
            setMessage "Data added"
        _ -> setMessage "Invalid data submitted"

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

showProfile :: ProfileData -> ORRoute -> Hamlet ORRoute
showProfile profile dest = $(hamletFile "show-profile")
