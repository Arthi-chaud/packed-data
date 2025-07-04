{-# LANGUAGE BangPatterns #-}

module Traversal () where

import Data.Packed
import Data.Packed.Reader
import qualified Data.Packed.Reader as R
import Tree

$(mkPacked ''Tree [])

myTree :: Tree Int
myTree = Node (Leaf 1) (Leaf 2)

packedTree :: Packed '[Tree Int]
packedTree = pack myTree

-- The two following functions do the same thing

sumPacked1 :: PackedReader '[Tree Int] r Int
sumPacked1 =
    caseTree
        ( R.do
            !n <- reader
            R.return n
        )
        ( R.do
            !left <- sumPacked1
            !right <- sumPacked1
            let !res = left + right
            R.return res
        )

sumPacked2 :: PackedReader '[Tree Int] r Int
sumPacked2 = PackedReader $ \case
    PackedLeaf l -> reader `with` l
    PackedNode n -> threadedWith n $ R.do
        !left <- sumPacked2
        !right <- sumPacked2
        let !res = left + right
        R.return res

runSum :: Int
runSum = fst $ runReader sumPacked1 packedTree
