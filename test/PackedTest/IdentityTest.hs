{-# LANGUAGE BangPatterns #-}

module PackedTest.IdentityTest (specs) where

import Data.Packed
import PackedTest.Data
import Test.Hspec

$(mkPacked ''Tree1 [])
$(mkPacked ''Tree2 [])
$(mkPacked ''Tree3 [])
$(mkPacked ''Tree4 [InsertFieldSize])
$(mkPacked ''MyData [])

specs :: Spec
specs = describe "Pack / Unpack Identity" $ do
    test "Tree 1" $ Node1 (Node1 (Leaf1 (1 :: Int)) (Leaf1 2)) (Leaf1 3)
    test "Tree 1 (with lists)" $ Node1 (Leaf1 [1 :: Int, 2]) (Node1 (Leaf1 []) (Leaf1 [3, 4]))
    test "Tree 1 (with custom ADT)" $ Node1 (Leaf1 (SmallData (1 :: Int))) (Node1 (Leaf1 $ BigData "Hello World") (Node1 (Leaf1 $ BigData "Goodbye") (Leaf1 $ SmallData 0)))
    test "Tree 1, size 20" $ buildNativeTree 20
    test "Tree 2" $ Node2 (Node2 Leaf2 (1 :: Int) Leaf2) 2 (Node2 Leaf2 3 Leaf2)
    test "Tree 3" $ Node3 (Node3 Leaf3 Leaf3 (1 :: Int)) (Node3 Leaf3 Leaf3 3) 2
    test "Tree 4" $ Node4 (Node4 (Leaf4 (1 :: Int)) (Leaf4 2)) (Leaf4 3)
  where
    test name tree =
        it name $
            let
                !ptree = pack tree
             in
                unpack' ptree `shouldBe` tree

buildNativeTree :: Int -> Tree1 Int
buildNativeTree 0 = Leaf1 1
buildNativeTree n = Node1 subTree subTree
  where
    !subTree = buildNativeTree (n - 1)
