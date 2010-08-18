{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod (liftIO, GHandler, MonadCatchIO, Html)
import Database.Persist
import Database.Persist.GenericSql
import Data.Time (UTCTime, getCurrentTime)
import Control.Arrow ((&&&))
import Control.Monad (liftM)

mkPersist [$persist|
Profile
    creation UTCTime
ProfileData
    profile ProfileId Eq
    name String
    value String
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

loadProfile :: MonadCatchIO m => ProfileId -> SqlPersist m [(String, String)]
loadProfile eid =
    map (profileDataName . snd &&& profileDataValue . snd)
        `liftM` selectList [ProfileDataProfileEq eid] [] 0 0

newUser :: MonadCatchIO m => String -> SqlPersist m UserId
newUser dn = do
    now <- liftIO getCurrentTime
    eid <- insert $ Profile now
    insert $ User now dn eid Nothing
