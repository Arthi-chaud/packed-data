-- | Showcase how to 'mutate' packed data
module Pack () where

import Data.Packed
import qualified Data.Packed.Needs as N
import Tree

$(mkPacked ''Tree [])

myTree :: Tree Int
myTree = Node (Leaf 1) (Leaf 2)

buildTree :: IO (Packed '[Tree Int])
buildTree =
    runBuilder $ \needs -> N.do
        node <- startNode needs
        leaf1 <- startLeaf node N.>>= write (1 :: Int)
        startLeaf leaf1 N.>>= write 2
