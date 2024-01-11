{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text hiding (map)

import ToolchainPerf
import ToolchainPerf.Graph
import ToolchainPerf.IO

-- ----------------------------------------------------------------------------
-- Options
-- ----------------------------------------------------------------------------

oFlags :: TestVariable
oFlags = ToolFlag ToolGhc "-O"
  [ "-O1"
  , "-O2"
  , "-O3"
  ]

ghcSystem :: TestVariable
ghcSystem = ToolVersion ToolGhc
  [ "ghc" ]

ghcVersions :: TestVariable
ghcVersions = ToolVersion ToolGhc
  [ "ghc-8.10.7"
  , "ghc-9.0.2"
  , "ghc-9.2.8"
  , "ghc-9.4.7"
  , "ghc-9.6.3"
  , "ghc-9.8.1"
  ]

cabalSystem :: TestVariable
cabalSystem = ToolVersion ToolCabal
  [ "cabal" ]

cabalVersions :: TestVariable
cabalVersions = ToolVersion ToolCabal
  [ "cabal-3.6.2.0"
  , "cabal-3.8.1.0"
  , "cabal-3.10.2.0"
  ]

-- ----------------------------------------------------------------------------
-- [GHC] Build Hello World
-- ----------------------------------------------------------------------------

ghc_build_helloWorld :: PerfTest
ghc_build_helloWorld = TestMatrix
  "tests/hello"
  ToolGhc
  [ "Hello.hs" ]
  [ ghcVersions
  , oFlags
  ]

-- ----------------------------------------------------------------------------
-- [System GHC, Cabal]: Build Hello World
-- ----------------------------------------------------------------------------

cabal_build_helloWorld :: PerfTest
cabal_build_helloWorld = TestMatrix
  "tests/hello"
  ToolCabal
  [ "build", "hello-world" ]
  [ ghcSystem
  , cabalVersions
  ]

-- ----------------------------------------------------------------------------
-- [Cabal, GHC] Build Cabal
-- ----------------------------------------------------------------------------

cabal_build_cabal :: PerfTest
cabal_build_cabal = TestMatrix
  "tests/cabal/cabal"
  ToolCabal
  [ "build", "Cabal" ]
  [ ghcVersions
  , cabalVersions
  ]

-- ----------------------------------------------------------------------------
-- [System Cabal, GHC] Build Simple Lenses
-- ----------------------------------------------------------------------------

cabal_build_simpleLenses_baseline :: PerfTest
cabal_build_simpleLenses_baseline = cabal_build_simpleLenses' "baseline"

cabal_build_simpleLenses_lenses :: PerfTest
cabal_build_simpleLenses_lenses = cabal_build_simpleLenses' "simple-lenses"

cabal_build_simpleLenses :: PerfTest
cabal_build_simpleLenses = TestName
  [ "baseline" $> cabal_build_simpleLenses_baseline
  , "lenses"   $> cabal_build_simpleLenses_lenses
  ]

cabal_build_simpleLenses' :: Text -> PerfTest
cabal_build_simpleLenses' target = TestMatrix
  "tests/simple-lenses"
  ToolCabal
  [ "build", target ]
  [ cabalSystem
  , ghcVersions
  , oFlags
  ]

-- ----------------------------------------------------------------------------
-- Tests
-- ----------------------------------------------------------------------------

infixr 1 $>

($>) :: a -> b -> (a, b)
($>) = (,)

tests :: PerfTest
tests = TestName
  [ "GHC build Hello World" $> ghc_build_helloWorld
  , "Cabal build Cabal"     $> cabal_build_cabal
  , "Simple lenses (TH)"    $> cabal_build_simpleLenses
  ]

-- ----------------------------------------------------------------------------
-- Script
-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  -- tests <- input auto "./tests/tests.dhall"
  savePerfResults "results/csv" =<< runPerfR (runPerfTest tests)

  -- where
  --   go names = \case
  --     ResultsName ns -> mapM_ (\(n, r) -> go (names ++ [n]) r) ns
  --     PerfResults args results -> graphPerfToFile names args results
