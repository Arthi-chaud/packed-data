module Types where

data BenchSuite = AST | Incr | Sum | RightMost deriving (Show)

type BenchmarkResults = [BenchmarkResult]

data BenchmarkResult = BenchmarkResult
    { suite :: BenchSuite
    , language :: String
    , name :: Maybe String
    , values :: [(Int, Double)]
    -- ^ Maps tree size to execution time, in ns
    }
    deriving (Show)
