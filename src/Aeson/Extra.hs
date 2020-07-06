module Aeson.Extra where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Casing as JSON
import Data.Char

dropFPrefixWithUnderscore :: String -> String
dropFPrefixWithUnderscore ('_':xs) = go xs
  where
    go [] = []
    go (x:xs)
      | isLower x = go xs
      | otherwise = (x : xs)
dropFPrefixWithUnderscore o = o

snakeCaseWithUnderscore ('_':xs) = '_' : JSON.snakeCase xs
snakeCaseWithUnderscore other = JSON.snakeCase other

jsonPrefix :: JSON.Options
jsonPrefix =
  JSON.defaultOptions
    { JSON.fieldLabelModifier =
        snakeCaseWithUnderscore . dropFPrefixWithUnderscore
    }
