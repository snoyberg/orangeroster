{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Controller
    ( withOR
    ) where

import Database.Persist.GenericSql
import Yesod
import Yesod.Helpers.Auth
import Yesod.Helpers.Static

-- Import all relevant handler modules here.
import Handler.Delete
import Handler.Entry
import Handler.Home
import Handler.Profile
import Handler.Note
import Handler.Root
import Handler.Share
import OR
import Model
import Settings

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in OR.hs. Please see
-- the comments there for more details.
mkYesodDispatch "OR" resourcesOR

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withOR :: (Application -> IO a) -> IO a
withOR f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    let h = OR s p
    toWaiApp h >>= f
  where
    s = static Settings.staticdir
