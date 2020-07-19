{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Control.Monad.Except
import qualified Data.Text as T

throwOnLeft :: MonadError String m => Either String a -> m a
throwOnLeft (Left err) = throwError err
throwOnLeft (Right v) = pure v

throwOnNothing :: MonadError String m => T.Text -> Maybe a -> m a
throwOnNothing err Nothing = throwError (T.unpack err)
throwOnNothing _ (Just v) = pure v
