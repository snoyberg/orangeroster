{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module OR
    ( OR (..)
    , ORRoute (..)
    , resourcesOR
    , Static
    , addUnverified'
    , Handler
    , loadProfile
    , ProfileData (..)
    , PTData (..)
    ) where

import Control.Monad (join)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (isJust)
import Database.Persist.GenericSql
import Network.Mail.Mime
import Safe (readMay)
import System.Directory
import Text.Blaze (toHtml)
import Yesod
import Yesod.Form.Jquery
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.Email
import Yesod.Helpers.Auth.Facebook
import Yesod.Helpers.Static

import Model
import qualified Settings
import StaticFiles

data OR = OR
    { getStatic :: Static
    , connPool :: Settings.ConnectionPool
    }

type Handler = GHandler OR OR

#if GHC7
mkYesodData "OR" [parseRoutes|
#else
mkYesodData "OR" [$parseRoutes|
#endif
/ RootR GET
/home HomeR GET POST
/profile/#UserId ProfileR GET
/display-name DisplayNameR POST

/share ShareR POST
/share/#UserId ShareUserR POST
/share/#UserId/stop StopShareUserR POST

/entries EntriesR POST
/entry/#EntryId EntryR GET POST
/entry/#EntryId/name EntryNameR POST
/entry/#EntryId/delete DeleteEntryR POST

/static StaticR Static getStatic
/favicon.ico FaviconR GET
/auth AuthR Auth getAuth

/note/#NoteId/close NoteCloseR POST

/data/phone/#PhoneId/delete DeletePhoneR POST
/data/address/#AddressId/delete DeleteAddressR POST
/data/screen-name/#ScreenNameId/delete DeleteScreenNameR POST
/data/misc/#MiscId/delete DeleteMiscR POST
|]

instance Yesod OR where
    approot _ = Settings.approot
    defaultLayout w = do
        u <- maybeAuth
        let user = fmap (userDisplayName . snd) u
        mmsg <- getMessage
        pc <- widgetToPageContent w
        hamletToRepHtml $(Settings.hamletFile "default-layout")
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static OR ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing
    authRoute _ = Just RootR
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        liftIO $ L.writeFile (statictmp ++ fn) content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

instance YesodPersist OR where
    type YesodDB OR = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodAuth OR where
    type AuthId OR = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR

    getAuthId c =
        case (credsPlugin c, credsIdent c, credsExtra c) of
            ("facebook", ci, cx) -> do
                x <- runDB $ getBy $ UniqueFacebook ci
                case lookup "verifiedEmail" cx of
                    Nothing -> return Nothing
                    Just email -> do
                        me <- runDB $ getBy $ UniqueEmail email
                        let dn = maybe "unknown user" id $ lookup "displayName" cx
                        uid <- case fmap (facebookCredUser . snd) x of
                            Just uid -> return uid
                            Nothing -> runDB $ do
                                uid <-
                                    case me of
                                        Just (_, Email (Just uid) _ _) -> return uid
                                        Just (eid, Email Nothing _ _) -> do
                                            uid <- newUser dn
                                            update eid [EmailOwner $ Just uid]
                                            return uid
                                        Nothing -> do
                                            uid <- newUser dn
                                            _ <- insert $ Email (Just uid) email Nothing
                                            return uid
                                _ <- insert $ FacebookCred uid ci
                                return uid
                        runDB $ claimShares uid email
                        return $ Just uid
            ("email", email, _) -> do
                uid <- runDB $ do
                    me <- getBy $ UniqueEmail email
                    uid <- case me of
                        -- FIXME maybe this should never happen?
                        Nothing -> do
                            uid <- newUser email
                            _ <- insert $ Email (Just uid) email Nothing
                            return uid
                        Just (_, Email (Just uid) _ _) -> return uid
                        Just (eid, Email Nothing _ _) -> do
                            uid <- newUser email
                            update eid [EmailOwner $ Just uid]
                            return uid
                    claimShares uid email
                    return uid
                return $ Just uid
            _ -> return Nothing
    showAuthId _ = show
    readAuthId _ = readMay
    authPlugins =
        [ authFacebook facebookKey facebookSecret ["email"]
        , authEmail ]

instance YesodAuthEmail OR where
    type AuthEmailId OR = EmailId
    showAuthEmailId _ = show
    readAuthEmailId _ = readMay
    addUnverified email verkey = runDB $ addUnverified' email verkey
    sendVerifyEmail email verkey verurl = do
        render <- getUrlRenderParams
        tm <- getRouteToMaster
        let lbs = renderHamlet render $(hamletFile "verify")
        liftIO $ renderSendMail Mail
            { mailHeaders =
                [ ("To", email)
                , ("From", "reply@orangeroster.com")
                , ("Subject", "OrangeRoster: Verify your email address")
                ]
            , mailParts = return $
                [ Part
                    { partType = "text/html; charset=utf-8"
                    , partEncoding = None
                    , partFilename = Nothing
                    , partContent = lbs
                    }
                ]
            }
    getVerifyKey eid = runDB $ do
        x <- get $ fromIntegral eid
        return $ maybe Nothing emailVerkey x
    setVerifyKey eid verkey = runDB $
        update (fromIntegral eid) [EmailVerkey $ Just verkey]
    verifyAccount eid = runDB $ do
        let emailid = fromIntegral eid
        x <- get emailid
        uid <-
            case x of
                Nothing -> return Nothing
                Just (Email (Just uid) _ _) -> return $ Just uid
                Just (Email Nothing email _) -> do
                    uid <- newUser email
                    update emailid [EmailOwner $ Just uid]
                    return $ Just uid
        update emailid [EmailVerkey Nothing]
        return uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword = \emailid' password -> runDB $ do
        let emailid = fromIntegral emailid'
        x <- get emailid
        case x of
            Just (Email (Just uid) _ _) -> do
                update uid [UserPassword $ Just password]
                update emailid [EmailVerkey Nothing]
            _ -> return ()
    getEmailCreds = \email -> runDB $ do
        x <- getBy $ UniqueEmail email
        case x of
            Nothing -> return Nothing
            Just (eid, e) ->
                return $ Just EmailCreds
                    { emailCredsId = fromIntegral eid
                    , emailCredsAuthId = emailOwner e
                    , emailCredsStatus = isJust $ emailOwner e
                    , emailCredsVerkey = emailVerkey e
                    }
    getEmail = \emailid -> runDB $ do
        x <- get $ fromIntegral emailid
        return $ fmap emailEmail x

instance YesodJquery OR where
    urlJqueryJs _ = Left $ StaticR jquery_js
    urlJqueryUiJs _ = Left $ StaticR jquery_ui_js
    urlJqueryUiCss _ = Left $ StaticR jquery_ui_css

addUnverified' :: String -> String -> SqlPersist (GHandler s OR) EmailId
addUnverified' email verkey = insert $ Email Nothing email (Just verkey)

data PTData = PTData
    { ptName :: String
    , ptValue :: Html
    , ptDelete :: ORRoute
    }

data ProfileData = ProfileData
    { pdPhone :: [PTData]
    , pdAddress :: [PTData]
    , pdScreenName :: [PTData]
    , pdMisc :: [PTData]
    }

loadProfile :: MonadInvertIO m => ProfileId -> SqlPersist m ProfileData
loadProfile eid = do
    phones <- selectList [PhoneProfileEq eid] [PhoneNameAsc, PhoneValueAsc] 0 0
    addresses <- selectList [AddressProfileEq eid] [AddressNameAsc, AddressValueAsc] 0 0
    screenNames <- selectList [ScreenNameProfileEq eid] [ScreenNameNameAsc, ScreenNameValueAsc] 0 0
    miscs <- selectList [MiscProfileEq eid] [MiscNameAsc, MiscValueAsc] 0 0
    return $ ProfileData
        (map goP phones)
        (map goA addresses)
        (map goS screenNames)
        (map goM miscs)
  where
    goP (k, Phone _ n v) = PTData n (toHtml v) (DeletePhoneR k)
    goA (k, Address _ n v) = PTData n (toHtml v) (DeleteAddressR k)
    goS (k, ScreenName _ n v) = PTData n (toHtml v) (DeleteScreenNameR k)
    goM (k, Misc _ n v) = PTData n (toHtml v) (DeleteMiscR k)
