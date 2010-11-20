{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)

import Data.Time

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    ident String
    password String null update
    onBacklog Bool update
    item ItemId null update
    freshBacklog Bool update
    changeBacklog Bool update
    UniqueUser ident
Email
    email String
    user UserId null update
    verkey String null update
    UniqueEmail email
Item
    title String
    owner UserId Eq
    timestamp UTCTime Gt Asc
    bubble Bool update Eq
    backlog Bool update Eq
    UniqueItem title owner
|]
