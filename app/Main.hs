{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Turtle.Prelude
import Control.Monad
import Control.Monad.IO.Class
import Data.Text as T
import Data.Csv
import ListT
import GHC.IO.Exception
import Data.ByteString.Lazy as BL
import System.Directory
import System.Clock

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
  
runTest :: Text -> Text -> ListT IO (TimeSpec, ExitCode)
runTest v o = do
  cleanup
  measureTime $ proc (ghcPathFor v) ["-v0", "-fforce-recomp", "Hello.hs", o] mempty

cleanup :: MonadIO m => m ()
cleanup = liftIO $ forM_ ["Hello.hi", "Hello.o", "Hello"] removeFile'

removeFile' :: MonadIO m => FilePath -> m ()
removeFile' f = liftIO $ doesFileExist f >>= \case
  True  -> removeFile f
  False -> return ()

-- ----------------------------------------------------------------------------
-- Script
-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  setCurrentDirectory "tests/hello"
  removeFile' "runs.csv"
  runs :: [[Text]] <- toList do
    v <- ghcVersions
    o <- oFlags
    liftIO $ print (v, o)
    fmap snd (runTest v o) >>= \case
      ExitSuccess -> do
        xs <- replicateM 10 (T.pack . show . toNanoSecs . fst <$> runTest v o)
        pure (v:o:xs)
      ExitFailure _ -> return []
  cleanup
  BL.writeFile "runs.csv" (encode runs)

ghcPathFor :: Text -> Text
ghcPathFor version = "ghc-" <> version

measureTime :: MonadIO m => m a -> m (TimeSpec, a)
measureTime m = do
  let get_time = liftIO (getTime ProcessCPUTime) 
  t1 <- get_time
  x <- m
  t2 <- get_time
  return (t2 `diffTimeSpec` t1, x)
