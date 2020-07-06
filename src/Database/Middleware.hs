{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Middleware where

import qualified Database as DB
import qualified Twitch.API as Twitch

class FromDatabase dbType appType | appType -> dbType where
  fromDatabase :: dbType -> appType

class ToDatabase appType dbType | appType -> dbType where
  toDatabase :: appType -> dbType

instance FromDatabase DB.Video Twitch.Video where
  fromDatabase (DB.Video {..}) =
    Twitch.Video
      { _vId = vVid
      , _vUserId = vUserId
      , _vUserName = vUserName
      , _vTitle = vTitle
      , _vDescription = vDescription
      , _vCreatedAt = vCreatedAt
      , _vPublishedAt = vPublishedAt
      , _vUrl = vUrl
      , _vThumbnailUrl = vThumbnailUrl
      , _vViewable = vViewable
      , _vViewCount = vViewCount
      , _vLanguage = vLanguage
      , _vType = vType
      , _vDuration = vDuration
      }

instance ToDatabase Twitch.Video DB.Video where
  toDatabase (Twitch.Video {..}) =
    DB.Video
      { vVid = _vId
      , vUserId = _vUserId
      , vUserName = _vUserName
      , vTitle = _vTitle
      , vDescription = _vDescription
      , vCreatedAt = _vCreatedAt
      , vPublishedAt = _vPublishedAt
      , vUrl = _vUrl
      , vThumbnailUrl = _vThumbnailUrl
      , vViewable = _vViewable
      , vViewCount = _vViewCount
      , vLanguage = _vLanguage
      , vType = _vType
      , vDuration = _vDuration
      }
