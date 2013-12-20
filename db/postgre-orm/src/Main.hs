{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Locale
import           Control.Monad.IO.Class (liftIO)
import           Data.Text
import           Data.Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    first       Text
    middle      Text        Maybe
    last        Text
    birthday    UTCTime
    phones      [Text]
    deriving Show
Address
    person      PersonId
    line1       Text
    line2       Text        Maybe
    city        Text
    state       Text
    zip         Int
    deriving Show
|]

connStr = "host=localhost dbname=ormTest user=ormTest password=ormTest port=5432"

main :: IO ()
main = withPostgresqlPool connStr 10 $ \pool -> do
        flip runSqlPersistMPool pool $ do
            runMigration migrateAll

            let johnBDay = readTime defaultTimeLocale "%d %b %Y" "05 Mar 1983"
            johnId <- insert $ Person "John" Nothing "Doe" johnBDay ["845-457-4965", "745-465-9575"]
            addr1Id <- insert $ Address johnId "234 W Something Dr." Nothing "Kansas City" "Missouri" 64854
            addr2Id <- insert $ Address johnId "63 S. Nothing Dr." (Just "Apt. 234") "Kansas City" "Kansas" 62458
            liftIO $ putStrLn (show johnId)
