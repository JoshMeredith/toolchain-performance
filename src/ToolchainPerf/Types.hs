{-# LANGUAGE OverloadedStrings #-}

module ToolchainPerf.Types where

import Data.Text
import Data.Map hiding (union)
import Dhall
import System.Clock

data PerfR
   = PerfR
   { rName :: [Text]
   , rOpts :: [Text]
   , rArgs :: Map ToolCommand [Text]
   , rTool :: Map ToolCommand Text
   }

data PerfTest
   = TestName [(Text, PerfTest)]
   | TestMatrix FilePath ToolCommand [Text] [TestVariable]

instance FromDhall PerfTest where
  autoWith i = union
    (  constructor "Test"   testName
    <> constructor "Matrix" testMatrix
    )
    where
      testName   = record $ TestName
        <$> field "tests" (autoWith i)
      testMatrix = record $ TestMatrix
        <$> field "testPath"  (autoWith i)
        <*> field "tool"      (autoWith i)
        <*> field "args"      (autoWith i)
        <*> field "variables" (autoWith i)

data PerfResults
   = ResultsName [(Text, PerfResults)]
   | PerfResults [Text] [([Text], [TimeSpec])]
   deriving Show

data TestVariable
   = ToolVersion ToolCommand [Text]
   | ToolFlag ToolCommand Text [Text]

instance FromDhall TestVariable where
  autoWith i = union
    (  constructor "Version" toolVersion
    <> constructor "Flag"    toolFlag
    )
    where
      toolVersion = record $ ToolVersion
        <$> field "tool"     (autoWith i)
        <*> field "versions" (autoWith i)
      toolFlag    = record $ ToolFlag
        <$> field "tool" (autoWith i)
        <*> field "name" (autoWith i)
        <*> field "opts" (autoWith i)

data ToolCommand
   = ToolGhc
   | ToolCabal
   -- | ToolGhcPkg "recache"
   deriving (Show, Eq, Ord, Enum)

instance FromDhall ToolCommand where
  autoWith _ = union
    (  constructor "GHC"   (const ToolGhc   <$> unit)
    <> constructor "Cabal" (const ToolCabal <$> unit)
    )
