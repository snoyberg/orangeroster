{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Form.Core
import Yesod.Form.Jquery
import OR
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
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Char (toLower, isSpace)

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
    , siEmail :: Maybe String
    }

gravatar :: String -> String
gravatar x =
    "http://www.gravatar.com/avatar/" ++ hash ++ "?d=wavatar&s=50"
  where
    hash = show $ md5 $ L.fromString $ map toLower $ trim x
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

getHomeR :: Handler RepHtml
getHomeR = do
    (uid, u) <- requireAuth
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
        let sf = st `elem` [ShareFrom, ShareBoth]
        email <-
            if sf
                then do
                    res <- selectList [EmailOwnerEq $ Just uid'] [] 1 0
                    case res of
                        (_, Email _ email _):_ -> return $ Just email
                        [] -> return Nothing
                else return Nothing
        return ShareInfo
            { siUid = uid'
            , siUser = u'
            , siShareTo = st `elem` [ShareTo, ShareBoth]
            , siShareFrom = sf
            , siEmail = email
            }
    entries <- runDB $ selectList [EntryOwnerEq uid] [EntryTitleAsc] 0 0
    notes <- runDB $ selectList [NoteUserEq uid] [NoteCreationDesc] 10 0 >>= mapM (\(nid, n) -> do
        ls <- selectList [NoteLinkNoteEq nid] [NoteLinkPriorityAsc] 0 0
        return ((nid, n), ls))
    y <- getYesod
    defaultLayout $ do
        setTitle "Homepage"
        let showProfile' = showProfile profile $ Just HomeR
        addHamlet $(hamletFile "home")
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlJqueryUiJs y
        addScript $ StaticR jquery_cookie_js
        addStylesheetEither $ urlJqueryUiCss y
        addCassius $(cassiusFile "home")
        addJulius $(juliusFile "home")

data PType = PTPhone | PTAddress | PTScreenName | PTMisc

postHomeR :: Handler ()
postHomeR = do
    (_, u) <- requireAuth
    insertProfile $ userProfile u
    redirect RedirectTemporary HomeR

insertProfile :: ProfileId -> Handler ()
insertProfile pid = do
    (x, _, _) <- runFormPostNoNonce entryForm
    case x of
        FormSuccess (pt, k, v) -> do
            case pt of
                PTPhone -> runDB $ insert (Phone pid k v) >> return ()
                PTAddress -> runDB $ insert (Address pid k $ Textarea v) >> return ()
                PTScreenName -> runDB $ insert (ScreenName pid k v) >> return ()
                PTMisc -> runDB $ insert (Misc pid k $ Textarea v) >> return ()
            setMessage "Data added"
        _ -> setMessage "Invalid data submitted"

postDisplayNameR :: Handler ()
postDisplayNameR = do
    (uid, _) <- requireAuth
    (res, _, _) <- runFormPostNoNonce $ stringInput "display-name"
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
