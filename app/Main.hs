{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Turtle.Prelude
import Data.Time.Clock
import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Csv
import ListT
import GHC.IO.Exception
import Data.ByteString.Lazy as BL

-- ----------------------------------------------------------------------------
-- Options
-- ----------------------------------------------------------------------------

oFlags :: Monad m => ListT m Text
oFlags = fromFoldable
  [ "-O1"
  , "-O2"
  , "-O3"
  ]

ghcVersions :: Monad m => ListT m Text
ghcVersions = fromFoldable
  [ "8.10.7"
  , "9.0.2"
  , "9.2.8"
  , "9.4.7"
  , "9.6.3"
  , "9.8.1"
  ]

-- ----------------------------------------------------------------------------
-- Test
-- ----------------------------------------------------------------------------
  
runTest :: Text -> Text -> ListT IO (NominalDiffTime, ExitCode)
runTest v o = do
  cleanup
  measureTime $ proc (ghcPathFor v) ["Hello.hs", o] mempty

cleanup :: MonadIO m => m ()
cleanup = void $ proc "rm" ["Hello.hi", "Hello.o", "Hello"] mempty

-- ----------------------------------------------------------------------------
-- Script
-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  runs :: [[String]] <- toList do
    v <- ghcVersions
    o <- oFlags
    liftIO $ print (v, o)
    fmap snd (runTest v o) >>= \case
      ExitSuccess -> replicateM 10 (show . (* 1000) . nominalDiffTimeToSeconds . fst <$> runTest v o)
      ExitFailure _ -> return []
  cleanup
  BL.writeFile "runs.csv" (encode runs)

ghcPathFor :: Text -> Text
ghcPathFor version = "ghc-" <> version

measureTime :: MonadIO m => m a -> m (NominalDiffTime, a)
measureTime m = do
  t1 <- liftIO getCurrentTime
  x <- m
  t2 <- liftIO getCurrentTime
  return (t2 `diffUTCTime` t1, x)
