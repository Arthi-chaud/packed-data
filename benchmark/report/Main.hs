module Main (main) where

import Control.Monad
import Data.List (intercalate)
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
    res <- runGoBenchmarks s
    print (values res)

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
