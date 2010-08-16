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
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql
import "MonadCatchIO-transformers" Control.Monad.CatchIO

hamletFile :: FilePath -> Q Exp
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile x = H.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"

juliusFile :: FilePath -> Q Exp
juliusFile x = H.juliusFileDebug $ "julius/" ++ x ++ ".julius"

connStr :: String
connStr = "user=orange password=orange host=localhost port=5432 dbname=orange"

withConnectionPool :: MonadCatchIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr 10

runConnectionPool :: MonadCatchIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

approot :: String
approot = "http://localhost:3000"

staticroot :: String
staticroot = approot ++ "/static"

staticdir :: FilePath
staticdir = "static"
