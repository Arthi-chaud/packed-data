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
    let benchRes = goRes ++ criterionRes
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
        ("C", Just "Inplace") -> 0
        ("C", Just "Produces new tree") -> 1
        ("C", _) -> 0
        ("Haskell", _) -> 2
        ("packed-data", Just "W/o Indirections") -> 3
        ("packed-data", Just "W/ Indirections") -> 4
        ("packed-data", Just "Non-monadic, w/o Indirections") -> 9
        ("packed-data", Just "Non-monadic, w/ Indirections") -> 10
        ("packed-data", Just "Unpacking, then traverse") -> 5
        ("packed-data", Just "Using NeedsBuilder") -> 6
        ("packed-data", Just "Unpacking, increment and repack") -> 7
        ("packed-data", Just "Deserialise and increment, and repack") -> 8
        ("Golang", Just "Inplace") -> 11
        ("Golang", Just "Produces new tree") -> 12
        ("Golang", _) -> 11
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
