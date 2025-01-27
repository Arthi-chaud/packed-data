module Go where

import Control.Monad (unless)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.IO hiding (stdout)
import System.Process
import Text.Read (readMaybe)
import Types
import Prelude

goTestPath :: String
goTestPath = "benchmark/report/go/benchmarks_test.go"

runGoBenchmarks :: BenchSuite -> IO BenchmarkResult
runGoBenchmarks s = do
    goIsFound <- doesFileExist goTestPath
    unless goIsFound $
        fail ("Could not find the Golang benchmark in " ++ goTestPath)
    (_, Just hout, _, _) <-
        createProcess (shell golangCmd){std_out = CreatePipe}
    hGetContents hout >>= parseOutput s
  where
    golangTestSuiteName = suiteToGolangBenchName s
    golangCmd = "go test " ++ goTestPath ++ " -bench " ++ golangTestSuiteName

suiteToGolangBenchName :: BenchSuite -> String
suiteToGolangBenchName = \case
    AST -> "BenchmarkAST"
    Sum -> "BenchmarkSum"
    RightMost -> "BenchmarkGetRightMost"
    Incr -> "BenchmarkIncrement"

parseOutput :: (MonadFail m) => BenchSuite -> String -> m BenchmarkResult
parseOutput s stdout = do
    parsedLines <- parseOutputLine `mapM` l
    return
        BenchmarkResult
            { suite = s
            , values = parsedLines
            , language = "Golang"
            , name = Nothing
            }
  where
    suiteName = suiteToGolangBenchName s
    l = filter (suiteName `isPrefixOf`) $ lines stdout

parseOutputLine :: (MonadFail m) => String -> m (Int, Double)
parseOutputLine bs =
    let res = do
            treeSize <- readMaybe rawTreeSize
            time <- readMaybe rawTime
            return (treeSize, time)
     in maybe (fail "Parsing error") return res
  where
    -- BenchmarkSum_5-10 -> 5
    rawTreeSize = takeWhile (/= '-') (drop 1 $ dropWhile (/= '_') bs)
    -- BenchmarkEval_5-10             	11866071	      101.7 ns/op -> 101.7
    rawTime =
        takeWhile (\c -> c == '.' || isDigit c) $
            dropWhile isSpace $
                dropWhile isDigit $
                    dropWhile isSpace $
                        dropWhile (not . isSpace) bs
