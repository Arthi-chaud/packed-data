-- | Showcase how to 'mutate' packed data
module Pack () where

import Data.Packed
import qualified Data.Packed.Needs as N
import Tree

$(mkPacked ''Tree [])

myTree :: Tree Int
myTree = Node (Leaf 1) (Leaf 2)

packedTree :: Packed '[Tree Int]
packedTree = pack myTree

buildTree :: Packed '[Tree Int]
buildTree =
    finish $
        withEmptyNeeds $ N.do
            startNode
            startLeaf N.>> write (1 :: Int)
            startLeaf N.>> write 2
