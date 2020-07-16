{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitch.Analytics where

import Aeson.Extra (jsonPrefix)
import Control.Monad.Except
import qualified Data.Aeson.TH as JSON
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Time.Format
import Data.Traversable (for)
import GHC.Generics
import qualified Twitch.API as Twitch
import qualified Twitch.Utils as Twitch

data VideoAnalytics =
  VideoAnalytics VideoPopularityTimeline
  deriving (Generic, Show, Eq)

data VideoTimeData =
  VideoTimeData
    { videoCreatedDate :: UTCTime
    , videoDuration :: NominalDiffTime
    }

data ClipTimeData =
  ClipTimeData
    { clipC :: Twitch.Clip
    , clipCreatedDate :: UTCTime
    , clipPosition :: NominalDiffTime
    , clipDuration :: NominalDiffTime
    }

data TimeData =
  TimeData
    { videoTimeData :: VideoTimeData
    , clipsTimeData :: [ClipTimeData]
    }

type VideoPopularityChange = Int

type VideoPopularityEvent = (Int, VideoPopularityChange)

data VideoPopularityTimeline =
  VideoPopularityTimeline
    { _vptDuration :: NominalDiffTime
    , _vptEvents :: [VideoPopularityEvent]
    }
  deriving (Eq, Show, Generic)

clipPopularityWeight :: Twitch.Clip -> Int
clipPopularityWeight = floor . logBase 2 . fromIntegral . Twitch._cViews

parseTimeData ::
     MonadError String m => Twitch.Video -> [Twitch.Clip] -> m TimeData
parseTimeData video clips = do
  clipsTimeData <-
    throwOnLeft $
    for clips $ \c -> do
      clipPosition <- Twitch.parseVodPositionOfClip c
      clipCreatedAt <-
        parseTimeM
          False
          defaultTimeLocale
          "%Y-%m-%dT%H:%M:%SZ"
          (T.unpack (Twitch._cCreatedAt c))
      let clipDuration = fromIntegral $ floor (Twitch._cDuration c)
      pure $ ClipTimeData c clipCreatedAt clipPosition clipDuration
  videoDuration <-
    throwOnLeft $ Twitch.parseTwitchVideoDuration (Twitch._vDuration video)
  videoCreatedDate <-
    throwOnNothing "Could not parse published date of video." $
    parseTimeM
      False
      defaultTimeLocale
      "%Y-%m-%dT%H:%M:%SZ"
      (T.unpack (Twitch._vPublishedAt video))
  let videoTimeData = VideoTimeData videoCreatedDate videoDuration
  pure $ TimeData videoTimeData clipsTimeData

serializeTimeline :: VideoPopularityTimeline -> [(Int, Int)]
serializeTimeline (VideoPopularityTimeline duration events) =
  consumeEvent (replicate (fromIntegral (floor duration)) (0, 0)) events
  where
    consumeEvent :: [(Int, Int)] -> [VideoPopularityEvent] -> [(Int, Int)]
    consumeEvent past [] = past
    consumeEvent (now@(t, popularity):future) (event@(event_t, popularity_change):future_events)
      | t == event_t =
        (t, popularity + popularity_change) : consumeEvent future future_events
      | otherwise = now : consumeEvent future (event : future_events)

analyse ::
     MonadError String m => Twitch.Video -> [Twitch.Clip] -> m VideoAnalytics
analyse video clips = do
  TimeData (VideoTimeData videoCreatedDate videoDuration) clipsTimeData <-
    parseTimeData video clips
  let vptEvents =
        [ event
        | ClipTimeData clip clip_created_date clip_position clip_duration <-
            sortOn clipCreatedDate clipsTimeData
        , let clip_start = fromInteger $ floor clip_position
        , let clip_end = clip_start + fromInteger (floor clip_duration)
        , event <-
            [ (clip_start, (clipPopularityWeight clip))
            , (clip_end, -(clipPopularityWeight clip))
            ]
        ]
  let timeline = VideoPopularityTimeline videoDuration vptEvents
  pure $ VideoAnalytics timeline

throwOnLeft :: MonadError String m => Either String a -> m a
throwOnLeft (Left err) = throwError err
throwOnLeft (Right v) = pure v

throwOnNothing :: MonadError String m => T.Text -> Maybe a -> m a
throwOnNothing err Nothing = throwError (T.unpack err)
throwOnNothing _ (Just v) = pure v

$(JSON.deriveJSON jsonPrefix ''VideoAnalytics)

$(JSON.deriveJSON jsonPrefix ''VideoPopularityTimeline)
