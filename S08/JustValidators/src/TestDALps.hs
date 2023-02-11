{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module TestDALps where

import          Control.Monad.IO.Class  (liftIO)
import          Database.Persist
-- import          Database.Persist.Sqlite
import          Database.Persist.TH
import          Database.Persist.Postgresql
import          Control.Monad.Logger    (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Students
    name String
    course String
    status String
    deriving Show
|]

dbConStr :: ConnectionString
dbConStr = "host=localhost port=5432 user=dev dbname=EA-NFT-CM password=1234" 


main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool dbConStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        jhonId <- insert $ Students "John Doe" "Fundamentals of Blockchain" "Ongoing"  
        janeId <- insert $ Students "Jane Doe" "Cardano Developer Professional" "FAILED"
        iaId <- insert $ Students "Infamous Alice" "Cardano Developer Professional" "PASS" 
        bobId <- insert $ Students "Bob" "Cardano Solution Architech" "Ongoing" 
        liftIO $ print (bobId)

showCDP :: IO ()
showCDP = runStderrLoggingT $ withPostgresqlPool dbConStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        listOfCDP <- selectList [StudentsCourse ==. "Cardano Developer Professional"] [] 
        liftIO $ print (listOfCDP)