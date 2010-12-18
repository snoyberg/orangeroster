{-# LANGUAGE PackageImports #-}
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , staticroot
    , staticdir
    , facebookKey
    , facebookSecret
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite
import Control.Monad.Invert

hamletFile :: FilePath -> Q Exp
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile x = H.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"

juliusFile :: FilePath -> Q Exp
juliusFile x = H.juliusFileDebug $ "julius/" ++ x ++ ".julius"

connStr :: String
connStr = "user=orange password=orange host=localhost port=5432 dbname=orange"

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool connStr 10

runConnectionPool :: MonadInvertIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

approot :: String
approot = "http://localhost:3000"

staticroot :: String
staticroot = approot ++ "/static"

staticdir :: FilePath
staticdir = "static"

facebookKey :: String
facebookKey = "5b9b0d6a39f24ca98a2ffdea4e30edee"

facebookSecret :: String
facebookSecret = "ee154153e840d0cef86b923a6931492a"
