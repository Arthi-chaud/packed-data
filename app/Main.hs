{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.DeepSeq
import Data.Fixed
import Data.Int
import Data.Packed
import qualified Data.Packed.Reader as R
import Data.Time.Clock
import Text.Printf

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)

$(mkPacked ''Tree1 [])

sumPacked :: PackedReader '[Tree1 Int64] r Int64
sumPacked =
    caseTree1
        ( R.do
            !n <- reader
            R.return n
        )
        ( R.do
            !left <- sumPacked
            !right <- sumPacked
            let !res = left + right
            R.return res
        )

buildNativeTree :: Int64 -> Tree1 Int64
buildNativeTree 0 = Leaf1 1
buildNativeTree n = Node1 subTree subTree
  where
    subTree = buildNativeTree (n - 1)

main :: IO ()
main = do
    let packed = pack $! buildNativeTree 15
    _ <- fromPacked packed `deepseq` return ()
    startTime <- getCurrentTime
    (!s, !_) <- R.runReader sumPacked packed
    endTime <- s `seq` getCurrentTime
    printf
        "Sum: %d. Total execution time: %ss.\n"
        s
        (showFixed True $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime)
