{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module App
    ( OR (..)
    , ORRoute (..)
    , resourcesOR
    , Static
    , reqUserId
    ) where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site
import Database.Persist.GenericSql
import Model
import Data.Time (getCurrentTime)

data OR = OR
    { getStatic :: Static
    , getAuth :: Auth
    , connPool :: Settings.ConnectionPool
    }

mkYesodData "OR" [$parseRoutes|
/ RootR GET
/home HomeR GET POST
/share ShareR POST
/profile/#UserId ProfileR GET

/static StaticR Static getStatic
/favicon.ico FaviconR GET
/auth AuthR Auth getAuth
|]

instance Yesod OR where
    approot _ = Settings.approot
    defaultLayout pc = do
        mmsg <- getMessage
        hamletToContent $(Settings.hamletFile "default-layout")
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static OR ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing
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
    defaultDest _ = HomeR
    defaultLoginRoute _ = RootR
    onLogin c _ =
        case (credsAuthType c, credsDisplayName c) of
            (AuthFacebook, Just dn) -> do
                let ci = credsIdent c
                x <- runDB $ getBy $ UniqueFacebook ci
                uid <- case fmap (facebookCredUser . snd) x of
                            Just uid -> return uid
                            Nothing -> do
                                now <- liftIO getCurrentTime
                                uid <- runDB $ insert $ User now dn
                                _ <- runDB $ insert $ FacebookCred uid ci
                                return uid
                setUserId uid
                case credsEmail c of
                    Nothing -> return ()
                    Just email -> runDB $ do
                        me <- getBy $ UniqueEmail email
                        case me of
                            Nothing -> do
                                _ <- insert $ Email uid email
                                return ()
                            Just _ -> do
                                -- FIXME check if this is for a different
                                -- account, and perhaps flag a merge request?
                                return ()
            _ -> return ()

userKey :: String
userKey = "USER"

intstring :: Integral i => i -> String
intstring i = show (fromIntegral i :: Int)

stringint :: Integral i => String -> Maybe i
stringint s = case reads s of
                (i, _):_ -> Just $ fromIntegral (i :: Int)
                [] -> Nothing

setUserId :: UserId -> GHandler s m ()
setUserId = setSession userKey . intstring

maybeUserId :: GHandler s OR (Maybe (UserId, User))
maybeUserId = do
    muid <- maybe Nothing stringint `fmap` lookupSession userKey
    case muid of
        Nothing -> return Nothing
        Just uid -> do
            mu <- runDB $ get uid
            case mu of
                Nothing -> return Nothing
                Just u -> return $ Just (uid, u)

reqUserId :: GHandler s OR (UserId, User)
reqUserId = do
    uid <- maybeUserId
    case uid of
        Just x -> return x
        Nothing -> do
            setMessage $ string "Please log in."
            setUltDest'
            redirect RedirectTemporary RootR
