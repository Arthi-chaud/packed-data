{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module OutputData (
    RawCSVEntry (..),
    BenchmarkMode (..),
    BenchmarkResult (..),
    BenchmarkValue (..),
    readBenchmarkResults,
    genFail,
) where

import qualified Data.ByteString.Lazy as BS
import Data.Char (isDigit)
import Data.Csv
import qualified Data.List.Safe as Safe
import Data.List.Split (splitOn)
import Data.Vector (toList)
import System.Exit
import Text.Read

data RawCSVEntry = RawCSVEntry
    { _name :: String
    , _mean :: Double
    }
instance FromNamedRecord RawCSVEntry where
    parseNamedRecord m = RawCSVEntry <$> m .: "Name" <*> m .: "Mean"

newtype BenchmarkMode = BenchmarkMode String deriving (Eq)

instance Show BenchmarkMode where
    show (BenchmarkMode s) = s

instance Read BenchmarkMode where
    readsPrec _ s = [(BenchmarkMode s, "")]

-- | Processed data
data BenchmarkResult = BenchmarkResult
    { name :: String
    -- ^ Example: Build
    , values :: [(Int, [BenchmarkValue])]
    -- ^ Maps benchmark values with the depths of the related tree.
    }
    deriving (Show)

data BenchmarkValue = BenchmarkValue
    { mode :: BenchmarkMode
    , meanExecutionTime :: Double
    }
    deriving (Show)

readBenchmarkResults :: FilePath -> IO [BenchmarkResult]
readBenchmarkResults csvPath = do
    rawFile <- BS.readFile csvPath
    rawCSVEntries <-
        either
            genFail
            (return . toList . snd)
            (decodeByName rawFile)
    processedCSVEntries <-
        either
            genFail
            return
            (processCSVEntries rawCSVEntries)
    return processedCSVEntries

processCSVEntries :: [RawCSVEntry] -> Either String [BenchmarkResult]
processCSVEntries rawEntries = do
    entries <-
        mapM
            (maybe (Left "Error when processing entry") Right . processCSVEntry)
            rawEntries
    return $ foldr mergeBenchmarkEntries [] entries

processCSVEntry :: RawCSVEntry -> Maybe BenchmarkResult
processCSVEntry (RawCSVEntry rawname mean) = do
    let splitEntryName = splitOn "/" rawname
    benchName <- splitEntryName Safe.!! (length splitEntryName - 3 :: Int)
    benchMode <- readMaybe =<< Safe.last splitEntryName
    depth <- do
        rawDepth <- splitEntryName Safe.!! (length splitEntryName - 2)
        readMaybe $ takeWhile isDigit rawDepth
    let benchValue =
            BenchmarkValue
                { mode = benchMode
                , meanExecutionTime = mean
                }
    return $
        BenchmarkResult
            { name = benchName
            , values = [(depth, [benchValue])]
            }

mergeBenchmarkEntries :: BenchmarkResult -> [BenchmarkResult] -> [BenchmarkResult]
mergeBenchmarkEntries benRes [] = [benRes]
mergeBenchmarkEntries benRes (mergedRes : mergedRest) =
    let benValue = head $ snd $ head (values benRes)
        depth = fst $ head (values benRes)
     in if name benRes == name mergedRes
            then mergedRes{values = mergeBenchmarkValues depth benValue (values mergedRes)} : mergedRest
            else mergedRes : mergeBenchmarkEntries benRes mergedRest
  where
    mergeBenchmarkValues :: Int -> BenchmarkValue -> [(Int, [BenchmarkValue])] -> [(Int, [BenchmarkValue])]
    mergeBenchmarkValues depth val [] = [(depth, [val])]
    mergeBenchmarkValues valDepth val ((depth, vals) : b) =
        if valDepth == depth
            then (depth, val : vals) : b
            else (depth, vals) : mergeBenchmarkValues valDepth val b

-- | Prints error message, and exits
genFail :: (Show a) => a -> IO b
genFail msg = do
    putStrLn "An error occured while generating graphs:"
    print msg
    exitFailure
