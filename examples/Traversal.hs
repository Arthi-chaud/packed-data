{-# LANGUAGE BangPatterns #-}

module Traversal () where

import Data.Packed
import qualified Data.Packed.Reader as R
import Tree

$(mkPacked ''Tree [])

myTree :: Tree Int
myTree = Node (Leaf 1) (Leaf 2)

packedTree :: IO (Packed '[Tree Int])
packedTree = pack myTree

sumPacked :: PackedReader '[Tree Int] r Int
sumPacked =
    caseTree
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

runSum :: IO Int
runSum = do
    (res, _) <- runReader sumPacked =<< packedTree
    return res
