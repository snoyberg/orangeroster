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
import Data.Monoid
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad (forM)
import Control.Arrow ((&&&))

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

data ShareType = ShareNone | ShareTo | ShareFrom | ShareBoth
    deriving Eq
instance Monoid ShareType where
    mempty = ShareNone
    mappend ShareNone x = x
    mappend x ShareNone = x
    mappend ShareTo ShareTo = ShareTo
    mappend ShareFrom ShareFrom = ShareFrom
    mappend _ _ = ShareBoth

data ShareInfo = ShareInfo
    { siUid :: UserId
    , siUser :: User
    , siShareTo :: Bool
    , siShareFrom :: Bool
    }

getHomeR :: Handler OR RepHtml
getHomeR = do
    (uid, u) <- reqUserId
    profile <- runDB $ loadProfile $ userProfile u
    emails <- runDB $ selectList [EmailOwnerEq $ Just uid] [] 0 0
    shares1 <- runDB $ selectList [ShareDestEq uid] [] 0 0 >>= mapM (\(_, Share srcid _) ->
        return (srcid, ShareFrom)
        )
    shares2 <- runDB $ selectList [ShareSourceEq uid] [] 0 0 >>= mapM (\(_, Share _ dstid) ->
        return (dstid, ShareTo)
        )
    let shares' = map (fst . head &&& map snd) $ groupBy ((==) `on` fst) $ sortBy (comparing fst)
                $ shares1 ++ shares2
    shares <- runDB $ forM shares' $ \(uid', st') -> do
        u' <- get404 uid'
        let st = mconcat st'
        return ShareInfo
            { siUid = uid'
            , siUser = u'
            , siShareTo = st `elem` [ShareTo, ShareBoth]
            , siShareFrom = st `elem` [ShareFrom, ShareBoth]
            }
    entries <- runDB $ selectList [EntryOwnerEq uid] [EntryTitleAsc] 0 0
    notes <- runDB $ selectList [NoteUserEq uid] [NoteCreationDesc] 10 0 >>= mapM (\(nid, n) -> do
        ls <- selectList [NoteLinkNoteEq nid] [NoteLinkPriorityAsc] 0 0
        return ((nid, n), ls))
    y <- getYesod
    applyLayoutW $ do
        setTitle "Homepage"
        let showProfile' = showProfile profile $ Just HomeR
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

showProfile :: ProfileData -> Maybe ORRoute -> Hamlet ORRoute
showProfile profile dest = do
    let profileTable rows = $(hamletFile "profile-table")
    $(hamletFile "show-profile")
