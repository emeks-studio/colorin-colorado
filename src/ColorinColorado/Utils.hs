{-# LANGUAGE LambdaCase #-}

module ColorinColorado.Utils 
  ( getOrThrow,
    orFail,
    tryRemoveFileIfExist
  ) where

import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (MonadIO)
import System.Directory (removeFile)

getOrThrow :: (MonadThrow m, MonadIO m) => m (Either String a) -> m a
getOrThrow x =
  x >>= \case
    Left err -> throwString err
    Right v -> pure v

orFail :: (MonadIO m) => Maybe a -> String -> m (Either String a)
orFail (Just v) _ = pure $ Right v
orFail Nothing msg = pure $ Left msg

tryRemoveFileIfExist :: FilePath -> IO ()
tryRemoveFileIfExist path =
  removeFile path `catch` \e -> if isDoesNotExistError e then return () else throwIO e
