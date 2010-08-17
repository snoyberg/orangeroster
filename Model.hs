{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod (liftIO, GHandler)
import Database.Persist
import Database.Persist.GenericSql
import Data.Time (UTCTime, getCurrentTime)
import Control.Arrow ((&&&))

mkPersist [$persist|
Profile
    creation UTCTime
ProfileData
    profile ProfileId Eq
    name String
    value String
User
    creation UTCTime
    displayName String
    profile ProfileId
FacebookCred
    user UserId
    ident String Eq
    UniqueFacebook ident
Email
    owner UserId Eq
    email String
    UniqueEmail email
Share
    source UserId
    dest UserId Eq
    UniqueShare source dest

Entry
    owner UserId Eq
    profile ProfileId
    title String Asc
|]

loadProfile :: ProfileId -> SqlPersist (GHandler s y) [(String, String)]
loadProfile eid =
    map (profileDataName . snd &&& profileDataValue . snd)
        `fmap` selectList [ProfileDataProfileEq eid] [] 0 0
