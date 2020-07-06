{-# LANGUAGE OverloadedStrings #-}

module Database.Extra where

import Database.Persist.Quasi
import qualified Data.Text as T
import Data.Aeson.Casing.Internal (dropFPrefix)
import Database.Persist.TH (persistWith, MkPersistSettings (..), sqlSettings)
import Data.Aeson.Casing

persistUnderscored = persistWith PersistSettings
  { psToDBName = T.pack . snakeCase . dropFPrefix . T.unpack
  , psStrictFields = True
  , psIdName = "id"
  }

appSqlSettings = sqlSettings { mpsPrefixFields = False }
