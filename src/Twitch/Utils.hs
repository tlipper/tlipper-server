{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Twitch.Utils where

import Control.Arrow ((>>>))
import Control.Monad.Except
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import qualified Twitch.API as Twitch
import Utils (throwOnNothing)

clipVodUrlRegex :: Regex
clipVodUrlRegex =
  makeRegex ("https://www.twitch.tv/videos/.+\\?t=(.+)" :: String)

parseVodPositionOfClip ::
     MonadError String m => Twitch.Clip -> m NominalDiffTime
parseVodPositionOfClip =
  Twitch._cVod >>>
  Twitch._cvUrl >>>
  T.unpack >>>
  match clipVodUrlRegex >>> \case
    MR {mrSubList = []} ->
      throwError "Could not find the video position of the clip."
    MR {mrSubList = (durationString:_)} ->
      parseTwitchVideoDuration $ T.pack durationString

parseTwitchVideoDuration :: MonadError String m => T.Text -> m NominalDiffTime
parseTwitchVideoDuration = liftEither . A.parseOnly parser
  where
    parser :: A.Parser NominalDiffTime
    parser = do
      hours <- A.option 0 $ A.decimal <* A.char 'h'
      minutes <- A.option 0 $ A.decimal <* A.char 'm'
      seconds <- A.decimal <* A.char 's'
      pure $ fromIntegral $ (hours * 3600) + (minutes * 60) + seconds
