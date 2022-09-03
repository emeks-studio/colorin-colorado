{-# LANGUAGE LambdaCase #-}

module ColorinColorado.Utils 
  ( getOrThrow,
    orFail
  ) where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (MonadIO)

getOrThrow :: (MonadThrow m, MonadIO m) => m (Either String a) -> m a
getOrThrow x =
  x >>= \case
    Left err -> throwString err
    Right v -> pure v

orFail :: (MonadIO m) => Maybe a -> String -> m (Either String a)
orFail (Just v) _ = pure $ Right v
orFail Nothing msg = pure $ Left msg