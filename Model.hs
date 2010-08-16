{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod (liftIO, GHandler)
import Database.Persist
import Database.Persist.GenericSql
import Data.Time (UTCTime, getCurrentTime)
import Control.Arrow ((&&&))

mkPersist [$persist|
User
    creation UTCTime
    displayName String
FacebookCred
    user UserId
    ident String Eq
    UniqueFacebook ident
Entry
    owner UserId Eq
    creation UTCTime
EntryData
    entry EntryId Eq
    name String
    value String
MainProfile
    user UserId
    entry EntryId
    UniqueMainProfile user
Email
    owner UserId Eq
    email String
    UniqueEmail email
Share
    source UserId
    dest UserId Eq
    UniqueShare source dest
|]

loadEntry :: EntryId -> SqlPersist (GHandler s y) [(String, String)]
loadEntry eid =
    map (entryDataName . snd &&& entryDataValue . snd)
        `fmap` selectList [EntryDataEntryEq eid] [] 0 0

getMainProfile :: UserId -> SqlPersist (GHandler s y) EntryId
getMainProfile uid = do
    x <- getBy $ UniqueMainProfile uid
    now <- liftIO getCurrentTime
    case x of
        Just (_, MainProfile _ eid) -> return eid
        Nothing -> do
            eid <- insert $ Entry uid now
            _ <- insert $ MainProfile uid eid
            return eid
