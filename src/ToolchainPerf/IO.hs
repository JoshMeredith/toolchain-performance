{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module ToolchainPerf.IO where

import Control.Monad
import Turtle.Prelude
import ToolchainPerf.Types
import Data.Text hiding (map, concatMap)
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import System.Clock
import System.FilePath


savePerfResults :: FilePath -> PerfResults -> IO ()
savePerfResults folder res = do
  void $ proc "mkdir" ["-p", pack folder] mempty
  go [] res

  where
    go names = \case
      ResultsName ns -> mapM_ (\(n, r) -> go (names ++ [n]) r) ns
      PerfResults _args results -> do
        BL.writeFile (folder </> (unpack $ intercalate "," names) <.> "csv") $
          encode $ concatMap encodeResult results
    encodeResult (tools, opts, times) =
      map (\t -> [intercalate ", " tools, intercalate ", " opts, encodeTime t]) times
    encodeTime = pack . show . (/ 1000000) . fromIntegral @_ @Double . toNanoSecs