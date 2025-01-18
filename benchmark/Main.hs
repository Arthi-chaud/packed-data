module Main (main) where

import qualified AST
import qualified Build
import CIReport
import Control.Monad (when)
import Criterion.Main
import Criterion.Main.Options (Mode (..), describe)
import Criterion.Types (Config (..))
import Data.Maybe (isJust, isNothing)
import qualified Increment
import qualified List
import Options.Applicative (execParser)
import qualified Pack
import Plot
import qualified Sum
import System.Directory
import qualified Traversals

main :: IO ()
main = do
    mode <- execParser (describe defaultConfig)
    when (isNothing (csvPath mode) && modeExportOtherThanCsv mode) $ do
        putStrLn "Warning: Graphs will not be generated."
        putStrLn "Warning: Report for CI will not be generated."
        putStrLn "Warning: Use the CSV export function to generate graphs and CI report."
    -- Criterion seems to append the result to the file if it already exists
    -- Therefore, it adds a new header. Cassava does not like this.
    _ <- case csvPath mode of
        Just p -> do
            exists <- doesFileExist p
            when exists (removeFile p)
        _ -> return ()
    runMode mode benchmarks
    case csvPath mode of
        Nothing -> return ()
        Just exportPath -> do
            putStrLn "Generating graph..."
            generateGraphs exportPath
            putStrLn "Generation finished."
            putStrLn "Generating CI report..."
            generateCIReport exportPath
            putStrLn "Generation CI report."
  where
    depths = [1, 5, 10, 15, 20]
    benchmarks =
        [ Traversals.benchmark depths
        , Pack.benchmark depths
        , Increment.benchmark depths
        , Sum.benchmark depths
        , AST.benchmark depths
        , Build.benchmark depths
        , List.benchmark depths
        ]
    csvPath (RunIters cfg _ _ _) = csvPathFromCfg cfg
    csvPath (Run cfg _ _) = csvPathFromCfg cfg
    csvPath _ = Nothing
    csvPathFromCfg = csvFile
    modeExportOtherThanCsv (RunIters cfg _ _ _) = cfgExportOtherThanCsv cfg
    modeExportOtherThanCsv (Run cfg _ _) = cfgExportOtherThanCsv cfg
    modeExportOtherThanCsv _ = False
    cfgExportOtherThanCsv cfg =
        any
            isJust
            $ ($ cfg)
                <$> [ junitFile
                    , jsonFile
                    , reportFile
                    , rawDataFile
                    ]
