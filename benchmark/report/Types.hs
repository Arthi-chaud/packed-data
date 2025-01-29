{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Csv hiding (lookup)
import Data.Maybe (fromJust)
import Numeric (showFFloat)

data BenchSuite = AST | Incr | Sum | RightMost deriving (Show)

type BenchmarkResults = [BenchmarkResult]

data BenchmarkResult = BenchmarkResult
    { suite :: BenchSuite
    , language :: String
    , name :: Maybe String
    , values :: [(Int, Double)]
    -- ^ Maps tree size to execution time, in s
    }
    deriving (Show)

instance ToNamedRecord BenchmarkResult where
    toNamedRecord (BenchmarkResult _ l n v) =
        namedRecord
            [ "Language" .= l
            , "Name" .= n
            , "1" .= get 1
            , "5" .= get 5
            , "10" .= get 10
            , "15" .= get 15
            , "20" .= get 20
            ]
      where
        get i = formatDuration (fromJust $ lookup i v)

formatDuration :: Double -> String
formatDuration d
    | d < (10 :: Double) ^^ (-6 :: Int) = showVal (d * 10 ^ (9 :: Int)) " ns"
    | d < (10 :: Double) ^^ (-3 :: Int) = showVal (d * 10 ^ (6 :: Int)) " us"
    | d < 1 = showVal (d * 10 ^ (3 :: Int)) " ms"
    | otherwise = showVal d " s"
  where
    showVal = showFFloat (Just 2)

instance DefaultOrdered BenchmarkResult where
    headerOrder _ = header ["Language", "Name", "1", "5", "10", "15", "20"]
