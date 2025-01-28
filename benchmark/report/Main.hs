module Main (main) where

import Control.Monad
import Criterion
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Csv
import Data.List (intercalate, sortBy)
import Go
import System.Directory
import System.Environment (getArgs)
import Types

main :: IO ()
main = do
    s <- getSuite
    isInStackProject <- doesFileExist "package.yaml"
    unless isInStackProject $
        fail "Please run this at the root of the packed-data project"
    goRes <- runGoBenchmarks s
    criterionRes <- runCriterionBenchmarks s
    let benchRes = goRes : criterionRes
        reportFileName = toLower <$> show s ++ "-report.csv"
    BS.writeFile reportFileName $
        encodeDefaultOrderedByName $
            orderBenchResults benchRes

orderBenchResults :: [BenchmarkResult] -> [BenchmarkResult]
orderBenchResults benchs = fst <$> sortBy (\(_, ia) (_, ib) -> ia `compare` ib) benchWithIndex
  where
    benchWithIndex = (\b -> (b, curry benchIndex (language b) (name b))) <$> benchs
    benchIndex :: (String, Maybe String) -> Int
    benchIndex = \case
        ("C", _) -> 0
        ("Haskell", _) -> 1
        ("packed-data", Just "W/o Indirections") -> 2
        ("packed-data", Just "W/ Indirections") -> 3
        ("packed-data", Just "Non-monadic, w/o Indirections") -> 8
        ("packed-data", Just "Non-monadic, w/ Indirections") -> 9
        ("packed-data", Just "Unpacking, then traverse") -> 4
        ("packed-data", Just "Using NeedsBuilder") -> 5
        ("packed-data", Just "Unpacking, increment and repack") -> 6
        ("packed-data", Just "Deserialise and increment, and repack") -> 7
        ("Golang", _) -> 10
        c -> error $ show c

getSuite :: IO BenchSuite
getSuite = do
    args <- getArgs
    case args of
        [s] -> parseSuite s
        _ -> fail "Expected exactly one argument"
  where
    parseSuite "sum" = return Sum
    parseSuite "ast" = return AST
    parseSuite "right-most" = return RightMost
    parseSuite "increment" = return Incr
    parseSuite _ =
        fail $
            "Expected one of ["
                ++ intercalate ", " ["ast", "sum", "right-most", "increment"]
                ++ "]"
