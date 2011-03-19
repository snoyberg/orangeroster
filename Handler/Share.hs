{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Share where

import Data.Time (getCurrentTime)
import Network.Mail.Mime
import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.Email

import OR
import Model
import Settings

--startShare :: UserId -> UserId -> Handle
startShare :: UserId -> User -> UserId -> Handler ()
startShare uid u dest = do
    runDB $ do
        _ <- insert $ Share uid dest
        let msg = userDisplayName u ++ " is now sharing with you."
        now <- liftIO getCurrentTime
        _ <- insert $ Note dest (string msg) now
        return ()
    setMessage "Sharing initiated"

postShareR :: Handler ()
postShareR = do
    (uid, u) <- requireAuth
    (res, _, _) <- runFormPostNoNonce $ emailInput "email"
    case res of
        FormSuccess email -> do
            x <- runDB $ getBy $ UniqueEmail email
            case x of
                Just (_, Email (Just dest) _ _) -> startShare uid u dest
                _ -> runDB $ do
                    _ <- insertBy $ ShareOffer uid email
                    lift $ setMessage "Sharing offer initiated"
                    y <- lift getYesod
                    verkey <- liftIO $ randomKey y
                    emailid <- addUnverified' email verkey
                    let url = AuthR $ PluginR "email"
                              ["verkey", show . fromPersistKey $ emailid
                              , show verkey ]
                    render <- lift getUrlRenderParams
                    let lbs = renderHamlet render $(hamletFile "invite")
                    liftIO $ renderSendMail Mail
                        { mailHeaders =
                            [ ("To", email)
                            , ("From", "noreply@orangeroster.com")
                            , ("Subject", "Invitation to OrangeRoster")
                            ]
                        , mailParts = return
                            [ Part
                                { partType = "text/html; charset=utf8"
                                , partEncoding = None
                                , partFilename = Nothing
                                , partContent = lbs
                                }
                            ]
                        }
        _ -> setMessage "Invalid email address submitted"
    redirect RedirectTemporary HomeR

postShareUserR :: UserId -> Handler ()
postShareUserR dest = do
    (uid, u) <- requireAuth
    startShare uid u dest
    redirect RedirectTemporary HomeR

postStopShareUserR :: UserId -> Handler ()
postStopShareUserR dest = do
    (uid, _) <- requireAuth
    runDB $ deleteBy $ UniqueShare uid dest
    setMessage "No longer sharing"
    redirect RedirectTemporary HomeR
