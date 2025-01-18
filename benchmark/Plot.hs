{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Plot (generateGraphs) where

import Control.Monad
import Data.List (nub)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import OutputData
import System.Directory (createDirectory, doesDirectoryExist)
import Text.Printf

-- | Generates and saves graphs using CSV exported by Criterion
generateGraphs :: FilePath -> IO ()
generateGraphs csvPath = do
    let outDir = "graphs"
    processedCSVEntries <- readBenchmarkResults csvPath
    outdirExists <- doesDirectoryExist outDir
    unless outdirExists $ createDirectory outDir
    forM_ processedCSVEntries (generateGraph outDir)

generateGraph :: FilePath -> BenchmarkResult -> IO ()
generateGraph outDir benRes = do
    -- TODO: Make this work in windows
    let outFile = printf "%s/%s.svg" outDir (name benRes)
    toFile def outFile $ do
        layout_title .= name benRes
        layout_x_axis . laxis_title .= "Depth of tree"
        layout_y_axis . laxis_title .= "Execution Time"
        forM_ benchTypes $ \m ->
            plot
                ( line
                    (show m)
                    [ [ (depth, meanExecutionTime val)
                      | (depth, vals) <- values benRes
                      , val <- vals
                      , mode val == m
                      ]
                    ]
                )
  where
    benchTypes = nub $ mode <$> concatMap snd (values benRes)
