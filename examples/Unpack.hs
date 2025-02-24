module Unpack () where

import Data.Packed
import Tree

$(mkPacked ''Tree [])

myTree :: Tree Int
myTree = Node (Leaf 1) (Leaf 2)

packedTree :: Packed '[Tree Int]
packedTree = pack myTree

unpackedTree :: Tree Int
unpackedTree = fst $ unpack packedTree
