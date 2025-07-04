{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}

module Increment (benchmark) where

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.List (intercalate)
import Data.Packed
import qualified Data.Packed.Needs as N
import qualified Data.Packed.Reader as R
import Data.Void
import Foreign
import Foreign.C
import GHC.Generics (Generic, Generic1)
import Utils
import Prelude hiding (concat)

foreign import capi unsafe "benchmark.h increment_inplace" c_increment_inplace :: Ptr Void -> IO ()

foreign import capi unsafe "benchmark.h increment" c_increment :: Ptr Void -> IO (Ptr Void)

foreign import capi unsafe "benchmark.h build_tree" c_build_tree :: CInt -> IO (Ptr Void)

foreign import capi unsafe "benchmark.h free_tree" c_free_tree :: Ptr Void -> IO ()
data Tree1 a = Leaf1 !a | Node1 !(Tree1 a) !(Tree1 a) deriving (Generic, Generic1)

instance (NFData a) => NFData (Tree1 a)
instance NFData1 Tree1

$(mkPacked ''Tree1 [])

benchmark :: [Int] -> Benchmark
benchmark depths =
    bgroup
        "increment"
        $ fmap computeTreeSumWithDepth depths

computeTreeSumWithDepth :: Int -> Benchmark
computeTreeSumWithDepth n =
    bgroup
        (depthGroupName n)
        [ envWithCleanup (c_build_tree (fromIntegral n)) c_free_tree $
            bench (cTestName ++ "-inplace") . nfAppIO c_increment_inplace
        , envWithCleanup (c_build_tree (fromIntegral n)) c_free_tree $
            bench (cTestName ++ "-new-tree") . nfAppIO c_increment
        , bench nativeTestName $
            nf increment nativeTree
        , bench (intercalate "-" [packedTestName, "needsbuilder"]) $
            nf incrementPackedRunner packedTree
        , bench (intercalate "-" ["unpack", "repack"]) $
            nf (pack . increment . fst . unpack) packedTree
        , bench (intercalate "-" [packedTestName, "rebuild-repack"]) $
            nf repackingIncrementPackedRunner packedTree
        ]
  where
    !packedTree = pack nativeTree
    !nativeTree = buildNativeTree n

increment :: Tree1 Int -> Tree1 Int
increment (Leaf1 n) = let !res = n + 1 in Leaf1 res
increment (Node1 t1 t2) = Node1 res1 res2
  where
    !res1 = increment t1
    !res2 = increment t2

-- Produces an needsbuilder for a tree alread incremented, and finishes it
incrementPackedRunner :: Packed '[Tree1 Int] -> Packed '[Tree1 Int]
incrementPackedRunner packed = runBuilder . fst $ runReader incrementPacked packed

incrementPacked :: PackedReader '[Tree1 Int] r (N.NeedsBuilder (Tree1 Int ': r1) '[Tree1 Int] r1 '[Tree1 Int])
incrementPacked =
    transformTree1
        ( R.do
            n <- reader
            R.return (write (n + 1))
        )
        ( R.do
            left <- incrementPacked
            right <- incrementPacked
            R.return (left N.>=> right)
        )

buildNativeTree :: Int -> Tree1 Int
buildNativeTree 0 = Leaf1 1
buildNativeTree n = Node1 subTree subTree
  where
    !subTree = buildNativeTree (n - 1)

-- Produces an unpacked tree alread incremented, and packs it
repackingIncrementPackedRunner :: Packed '[Tree1 Int] -> Packed '[Tree1 Int]
repackingIncrementPackedRunner packed =
    let
        (!tree, _) = runReader repackingIncrementPacked packed
        !repacked = pack tree
     in
        repacked

repackingIncrementPacked :: PackedReader '[Tree1 Int] r (Tree1 Int)
repackingIncrementPacked =
    caseTree1
        ( R.do
            n <- reader
            R.return (Leaf1 (n + 1))
        )
        ( R.do
            !left <- repackingIncrementPacked
            !right <- repackingIncrementPacked
            R.return (Node1 left right)
        )
