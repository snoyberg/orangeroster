{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Yesod (liftIO, Html, Textarea)
import Database.Persist
import Database.Persist.GenericSql
import Database.Persist.TH (share2)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.IO.Peel

#if GHC7
share2 mkPersist (mkMigrate "migrateAll") [persist|
#else
share2 mkPersist (mkMigrate "migrateAll") [$persist|
#endif
Profile
    creation UTCTime
User
    creation UTCTime
    displayName String Update
    profile ProfileId
    password String Maybe Update
    UniqueUserProfile profile
FacebookCred
    user UserId
    ident String Eq
    UniqueFacebook ident
Email
    owner UserId Maybe Eq Update
    email String
    verkey String Maybe Update
    UniqueEmail email
Share
    source UserId Eq
    dest UserId Eq
    UniqueShare source dest
ShareOffer
    source UserId
    dest String Eq
    UniqueShareOffer source dest

Phone
    profile ProfileId Eq
    name String Asc
    value String Asc
Address
    profile ProfileId Eq
    name String Asc
    value Textarea Asc
ScreenName
    profile ProfileId Eq
    name String Asc
    value String Asc
Misc
    profile ProfileId Eq
    name String Asc
    value Textarea Asc

Entry
    owner UserId Eq
    profile ProfileId
    title String Asc Update
    UniqueEntryProfile profile

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

newUser :: (MonadPeelIO m, PersistBackend m) => String -> m UserId
newUser dn = do
    now <- liftIO getCurrentTime
    eid <- insert $ Profile now
    insert $ User now dn eid Nothing

claimShares :: MonadPeelIO m => UserId -> String -> SqlPersist m ()
claimShares uid email = do
    selectList [ShareOfferDestEq email] [] 0 0 >>= mapM_ (\(sid, s) -> do
        _ <- insert $ Share (shareOfferSource s) uid
        delete sid)
