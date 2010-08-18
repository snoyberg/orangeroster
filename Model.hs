{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod (liftIO, MonadCatchIO, Html, Textarea)
import Database.Persist
import Database.Persist.GenericSql
import Data.Time (UTCTime, getCurrentTime)
import Control.Arrow ((&&&))
import Control.Monad (liftM)

mkPersist [$persist|
Profile
    creation UTCTime
User
    creation UTCTime
    displayName String update
    profile ProfileId
    password String null update
FacebookCred
    user UserId
    ident String Eq
    UniqueFacebook ident
Email
    owner UserId Eq
    email String
    verkey String null update
    verified Bool update
    UniqueEmail email

Share
    source UserId
    dest UserId Eq
    UniqueShare source dest
ShareOffer
    source UserId
    dest String Eq
    UniqueShareOffer source dest

Phone
    profile ProfileId Eq
    name String
    value String
Address
    profile ProfileId Eq
    name String
    value Textarea
ScreenName
    profile ProfileId Eq
    name String
    value String
Misc
    profile ProfileId Eq
    name String
    value Textarea

Entry
    owner UserId Eq
    profile ProfileId
    title String Asc

Note
    user UserId Eq
    content Html
    creation UTCTime Desc
    deriving
NoteLink
    note NoteId Eq
    dest String
    text Html
    priority Int Asc
    deriving
|]

data ProfileData = ProfileData
    { pdPhone :: [(String, String)]
    , pdAddress :: [(String, Textarea)]
    , pdScreenName :: [(String, String)]
    , pdMisc :: [(String, Textarea)]
    }

loadProfile :: MonadCatchIO m => ProfileId -> SqlPersist m ProfileData
loadProfile eid = do
    phones <- map (phoneName . snd &&& phoneValue . snd)
                `liftM` selectList [PhoneProfileEq eid] [] 0 0
    addresses <- map (addressName . snd &&& addressValue . snd)
                `liftM` selectList [AddressProfileEq eid] [] 0 0
    screenNames <- map (screenNameName . snd &&& screenNameValue . snd)
                `liftM` selectList [ScreenNameProfileEq eid] [] 0 0
    miscs <- map (miscName . snd &&& miscValue . snd)
                `liftM` selectList [MiscProfileEq eid] [] 0 0
    return $ ProfileData phones addresses screenNames miscs

newUser :: MonadCatchIO m => String -> SqlPersist m UserId
newUser dn = do
    now <- liftIO getCurrentTime
    eid <- insert $ Profile now
    insert $ User now dn eid Nothing
