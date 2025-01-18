{-# LANGUAGE DeriveGeneric #-}

module CIReport (generateCIReport) where

import Data.Aeson
import GHC.Generics
import OutputData (BenchmarkResult (BenchmarkResult), BenchmarkValue (meanExecutionTime), mode, readBenchmarkResults)
import Text.Printf
import Utils (nativeTestName)

data ReportEntry = ReportEntry
    { name :: String
    , unit :: String
    , value :: Double
    }
    deriving (Generic, Show)

instance ToJSON ReportEntry

generateCIReport :: FilePath -> IO ()
generateCIReport csvPath = do
    let reportPath = "benchmark-report.json"
    benRes <- readBenchmarkResults csvPath
    let reportEntries = concatMap toReportEntries benRes
    encodeFile reportPath reportEntries

toReportEntries :: BenchmarkResult -> [ReportEntry]
toReportEntries (BenchmarkResult benName values) =
    concatMap
        ( \(depth, depthValues) ->
            ( \depthVal ->
                ReportEntry
                    { name = printf "%s/%d (%s)" benName depth (show $ mode depthVal)
                    , unit = "seconds"
                    , value = meanExecutionTime depthVal
                    }
            )
                <$> filter (\v -> show (mode v) /= nativeTestName) depthValues
        )
        values
