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

instance FromDatabase DB.Channel Twitch.Channel where
  fromDatabase (DB.Channel {..}) =
    Twitch.Channel
      { _chBroadcasterLanguage = chBroadcasterLanguage
      , _chDisplayName = chDisplayName
      , _chGameId = chGameId
      , _chId = chChId
      , _chIsLive = chIsLive
      , _chThumbnailUrl = chThumbnailUrl
      , _chTitle = chTitle
      , _chStartedAt = chStartedAt
      }

instance ToDatabase Twitch.Channel DB.Channel where
  toDatabase (Twitch.Channel {..}) =
    DB.Channel
      { chBroadcasterLanguage = _chBroadcasterLanguage
      , chDisplayName = _chDisplayName
      , chGameId = _chGameId
      , chChId = _chId
      , chIsLive = _chIsLive
      , chThumbnailUrl = _chThumbnailUrl
      , chTitle = _chTitle
      , chStartedAt = _chStartedAt
      }

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

instance FromDatabase DB.Clip Twitch.Clip where
  fromDatabase (DB.Clip {..}) =
    Twitch.Clip
      { _cSlug = cSlug
      , _cTrackingId = cTrackingId
      , _cUrl = cUrl
      , _cEmbedUrl = cEmbedUrl
      , _cEmbedHtml = cEmbedHtml
      , _cVod = Twitch.ClipVod cVodId cVodUrl
      , _cGame = cGame
      , _cLanguage = cLanguage
      , _cTitle = cTitle
      , _cViews = cViews
      , _cDuration = cDuration
      , _cCreatedAt = cCreatedAt
      }

instance ToDatabase Twitch.Clip DB.Clip where
  toDatabase (Twitch.Clip {..}) =
    DB.Clip
      { cSlug = _cSlug
      , cTrackingId = _cTrackingId
      , cUrl = _cUrl
      , cEmbedUrl = _cEmbedUrl
      , cEmbedHtml = _cEmbedHtml
      , cVodId = Twitch._cvId _cVod
      , cVodUrl = Twitch._cvUrl _cVod
      , cGame = _cGame
      , cLanguage = _cLanguage
      , cTitle = _cTitle
      , cViews = _cViews
      , cDuration = _cDuration
      , cCreatedAt = _cCreatedAt
      }
