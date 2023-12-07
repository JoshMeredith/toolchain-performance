{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module ToolchainPerf (
  module ToolchainPerf.Types,

  invoke,
  runPerfTest,
  runPerfR,

  measureTime,
) where

import ToolchainPerf.Types
import Control.Monad.Reader
import System.Clock
import Control.Monad.Trans.Maybe
import Data.Map as Map hiding (map, toList)
import Data.Text as Text hiding (map, replicate)
import Turtle.Prelude
import GHC.IO.Exception
import ListT
import Data.Maybe
import System.Directory
import Control.Monad.Trans.Control


runPerfR :: ReaderT PerfR m a -> m a
runPerfR m = runReaderT m newPerfR


newPerfR :: PerfR
newPerfR = PerfR mempty mempty mempty mempty


runPerfTest
  :: (MonadBaseControl IO m, MonadReader PerfR m, MonadIO m)
  => PerfTest
  -> m PerfResults

runPerfTest (TestName ts) = ResultsName <$> forM ts \(n, t) -> do
  t' <- local (addName n) (runPerfTest t)
  return (n, t')
  where
    addName name r = r{rName = rName r <> [name]}

runPerfTest (TestMatrix dir cmd args vs) = control \runInBase ->
  withCurrentDirectory dir $ runInBase do
    PerfResults (matrixHeader vs) <$> runMatrix 1 vs cmd args


matrixHeader :: [TestVariable] -> [Text]
matrixHeader = map variableName


variableName :: TestVariable -> Text
variableName = \case
  ToolVersion ToolGhc     _ -> "GHC"
  ToolVersion ToolCabal   _ -> "Cabal"
  ToolFlag    _         n _ -> n


runMatrix
  :: (MonadIO m, MonadReader PerfR m)
  => Int
  -> [TestVariable]
  -> ToolCommand
  -> [Text]
  -> m [([Text], [TimeSpec])]
runMatrix n vars cmd cmdArgs = toList $ go [] vars
  where
    go :: (MonadIO m, MonadReader PerfR m)
       => [Text] -> [TestVariable] -> ListT m ([Text], [TimeSpec])
    go args [] = invoke cmd cmdArgs >>= \case
      Nothing -> return (args, replicate n 0)
      Just _  -> (args,) <$> do
        replicateM n do
          x <- invoke cmd cmdArgs
          return $ fromMaybe 0 x
    go args (v:vs) = case v of
      ToolVersion t   tvs -> do
        tv <- fromFoldable tvs
        local (setToolVersion t tv) $ go (args ++ [tv]) vs
      ToolFlag    t _ tfs -> do
        tf <- fromFoldable tfs
        local (addToolArg t tf) $ go (args ++ [tf]) vs


setToolVersion :: ToolCommand -> Text -> PerfR -> PerfR
setToolVersion t v r = r { rTool = insert t v (rTool r) }


addToolArg :: ToolCommand -> Text -> PerfR -> PerfR
addToolArg t v r = r { rArgs = insert t (args ++ [v]) (rArgs r) }
  where
    args = fromMaybe [] . Map.lookup t $ rArgs r


measureTime :: MonadIO m => m ExitCode -> MaybeT m TimeSpec
measureTime m = do
  let get_time = liftIO $ getTime Realtime
  t1 <- get_time
  e <- MaybeT $ Just <$> m
  t2 <- get_time
  case e of
    ExitSuccess   -> return $ t2 - t1
    ExitFailure _ -> MaybeT $ return Nothing


lookupTool :: MonadReader PerfR m => ToolCommand -> MaybeT m Text
lookupTool t = MaybeT $ Map.lookup t . rTool <$> ask


lookupArgs :: MonadReader PerfR m => ToolCommand -> MaybeT m [Text]
lookupArgs t = fromMaybe [] . Map.lookup t . rArgs <$> ask


escapeArgs :: [Text] -> Text
escapeArgs args = Text.concat
  [ "\""
  , intercalate " " $ map escapeArg args
  , "\""
  ]
  where
    escapeArg = Text.concatMap \case
      '"'  -> "\\\""
      '\\' -> "\\"
      c    -> Text.singleton c


invoke
  :: (MonadIO m, MonadReader PerfR m)
  => ToolCommand
  -> [Text]
  -> m (Maybe TimeSpec)

invoke ToolGhc args = do
  void $ proc "mkdir" ["-p", "tmp"] mempty
  mt <- runMaybeT do
    ghc_cmd  <- lookupTool ToolGhc
    ghc_args <- lookupArgs ToolGhc
    liftIO $ print $ intercalate " " $ join [[ghc_cmd], ghc_args, args]
    measureTime do
      proc ghc_cmd 
        (ghc_args ++ ["-o", "tmp/a.out", "-hidir", "tmp", "-odir", "tmp"] ++ args)
        mempty
  void $ proc "rm" ["-r", "tmp"] mempty
  return mt

invoke ToolCabal args = runMaybeT do
  ghc_cmd    <- lookupTool ToolGhc
  cabal_cmd  <- lookupTool ToolCabal
  ghc_args   <- lookupArgs ToolGhc
  cabal_args <- lookupArgs ToolCabal
  liftIO $ print $ (cabal_cmd, cabal_args)
  t <- measureTime do
    proc cabal_cmd
      (cabal_args ++ ["--with-compiler=" <> ghc_cmd, "--ghc-options=" <> escapeArgs ghc_args] ++ args)
      mempty
  void $ proc "rm" ["-r", "dist-newstyle"] mempty
  return t


