{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ToolchainPerf.Graph where

import Data.Default.Class
import Graphics.Rendering.Chart
import Data.Text hiding (map, take, length, drop)
import System.Clock
import Control.Lens
import Control.Monad.State
import Statistics.Sample
import Data.Vector (fromList)
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour hiding (over)
import Data.Colour.Names


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
    stdevs :: [(String, (Double, Double))]
    stdevs
      = results
      & map (over (_2 . traverse) ((/ 1000000) . fromIntegral . toNanoSecs)) -- milliseconds :: Double
      & map (over _2 fromList)
      & map (\(l, ts) -> (show l, (harmonicMean ts, stdDev ts))) 

    vals = map (\(l, (m, _e)) -> [(m, l)]) stdevs

    bars = def & execState do
      plot_bars_values_with_labels .= addIndexes vals
      plot_bars_label_angle .= (-60)
      plot_bars_label_text_hanchor .= HTA_Left

    errors
      = map snd stdevs
      & addIndexes
      & map (\(idx, (v, s)) -> symErrPoint idx v 0 s)

    error_lines = def & execState do
      line_color .= opaque red

    error_bars = def & execState do
      plot_errbars_values     .= errors
      plot_errbars_line_style .= error_lines

  -- layout_x_axis . laxis_generate .= \xs -> makeAxis (\ys -> take (length ys) xlabels) (xs, xs, xs)
  -- layout_x_axis . laxis_generate .= autoIndexAxis (map (take 3 . drop 1) xlabels)

  layout_plots .= [ plotBars bars, toPlot error_bars ]
