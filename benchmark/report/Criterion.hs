module Criterion where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Scientific
import Debug.Trace (trace)
import System.Directory (removeFile)
import System.Process
import Text.Read (readMaybe)
import Types

runCriterionBenchmarks :: BenchSuite -> IO BenchmarkResults
runCriterionBenchmarks s = do
    (_, _, _, h) <- createProcess (shell criterionCmd)
    waitForProcess h
    csv <- readFile reportPath
    parseCSV s csv
  where
    reportPath = "report.csv"
    criterionTestSuiteName = suiteToCriterionBenchName s
    criterionCmd =
        "cabal bench tree-bench --benchmark-options='--csv "
            ++ reportPath
            ++ " "
            ++ criterionTestSuiteName
            ++ "'"

suiteToCriterionBenchName :: BenchSuite -> String
suiteToCriterionBenchName = \case
    AST -> "ast"
    Sum -> "sum"
    RightMost -> "traversals/right-most"
    Incr -> "increment"

parseCSV :: BenchSuite -> String -> IO BenchmarkResults
parseCSV s csv = do
    let cleanCSV = drop 1 $ lines csv
    res <- parseCSVForCase cleanCSV s `mapM` cases
    return $ filter (not . null . values) res

parseCSVForCase :: [String] -> BenchSuite -> (String, String, Maybe String) -> IO BenchmarkResult
parseCSVForCase csvLines s (caseId, lang, subtitle) = do
    let caseLines = filter (('/' : caseId ++ ",") `isInfixOf`) csvLines
        benchName = suiteToCriterionBenchName s
    caseValues <- parseCSVLineForCase benchName `mapM` caseLines
    return $ BenchmarkResult s lang subtitle caseValues

parseCSVLineForCase :: (MonadFail m) => String -> String -> m (Int, Double)
parseCSVLineForCase caseId line =
    let res = do
            treeSize <- readMaybe rawTreeSize
            time <- readMaybe rawTime :: Maybe Scientific
            return (treeSize, toRealFloat time)
     in maybe (fail "Parsing error") return res
  where
    rawTime =
        takeWhile (/= ',') $
            drop 1 $
                dropWhile (/= ',') line
    rawTreeSize =
        takeWhile isDigit $
            drop 1 $
                drop (length caseId) line

cases :: [(String, String, Maybe String)]
cases =
    [ ("c", "C", Nothing)
    , ("native", "Haskell", Nothing)
    , ("packed", "packed-data", Just "W/o Indirections")
    , ("packed-with-size", "packed-data", Just "W/ Indirections")
    , ("packed-unpacked", "packed-data", Just "Unpacking, then traverse")
    , ("non-monadic-packed", "packed-data", Just "Non-monadic, w/o indirections")
    , ("non-monadic-packed-with-size", "packed-data", Just "Non-monadic, w indirections")
    , ("packed-needsbuilder", "packed-data", Just "Using NeedsBuilder")
    , ("unpack-repack", "packed-data", Just "Unpacking, increment and repack")
    , ("packed-rebuild-repack", "packed-data", Just "Deserialise and increment, and repack")
    ]
