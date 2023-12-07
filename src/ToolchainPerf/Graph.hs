{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ToolchainPerf.Graph where

import Data.Default.Class
import Graphics.Rendering.Chart
import Data.Text hiding (map)
import System.Clock
import Control.Lens
import Control.Monad.State
import Statistics.Sample
import Data.Vector (fromList)
import Graphics.Rendering.Chart.Backend.Diagrams


graphPerfToFile :: [Text] -> [Text] -> [([Text], [TimeSpec])] -> IO ()
graphPerfToFile testName args results
  = graphPerf args results
  & toRenderable
  & renderableToFile def fp
  & void
  where
    fp = unpack $ intercalate "." testName <> ".svg"


newToolGraph :: (PlotValue x, PlotValue y) => Layout x y
newToolGraph = def & execState do
  return ()


graphPerf :: [Text] -> [([Text], [TimeSpec])] -> Layout PlotIndex Double
graphPerf _args results = newToolGraph & execState do

  let
    stdevs :: [(Double, Double)]
    stdevs = results
           & map snd
           & map (map $ (/ 1000000) . fromIntegral . toNanoSecs) -- milliseconds :: Double
           & map fromList 
           & map (\ts -> (harmonicMean ts, stdDev ts)) 

    vals = fst $ unzip stdevs

    bars = def & execState do
      plot_bars_values .= addIndexes (map (:[]) vals)

    errors = map (\(idx, (v, s)) -> symErrPoint idx v 0 s) $ addIndexes stdevs

    error_bars = def & execState do
      plot_errbars_values .= errors

  layout_plots .= [ plotBars bars, toPlot error_bars ]
