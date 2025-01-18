module PackedTest.UnpackTest (specs) where

import ByteString.StrictBuilder
import Data.Int (Int32)
import Data.Packed
import Data.Packed.TH
import PackedTest.Data
import Test.Hspec

$(mkPacked ''Tree1 [])
$(mkPacked ''Tree2 [])
$(mkPacked ''Tree3 [])
$(mkPacked ''Tree4 [InsertFieldSize])

specs :: Spec
specs = describe "Unpack Trees" $ do
    describe "Tree1" $ do
        test
            "Leaf"
            (storable (0 :: Tag) <> storable (10 :: Int))
            (Leaf1 10 :: Tree1 Int)
        test
            "Node"
            ( let node = storable (1 :: Tag) <> subNode <> leaf
                  subNode = storable (1 :: Tag) <> subLeafL <> subLeafR
                  subLeafL = storable (0 :: Tag) <> storable (1 :: Int)
                  subLeafR = storable (0 :: Tag) <> storable (2 :: Int)
                  leaf = storable (0 :: Tag) <> storable (3 :: Int)
               in node
            )
            (Node1 (Node1 (Leaf1 1) (Leaf1 2)) (Leaf1 3) :: Tree1 Int)
    describe "Test2" $ do
        test
            "Leaf"
            (storable (0 :: Tag))
            (Leaf2 :: Tree2 Int)
        test
            "Node"
            ( let node = storable (1 :: Tag) <> leaf <> storable (1 :: Int) <> leaf
                  leaf = storable (0 :: Tag)
               in node
            )
            (Node2 Leaf2 (1 :: Int) Leaf2)
    describe "Test3" $ do
        test
            "Leaf"
            (storable (0 :: Tag))
            (Leaf3 :: Tree3 Int)
        test
            "Node"
            ( let node = storable (1 :: Tag) <> leaf <> leaf <> storable (42 :: Int)
                  leaf = storable (0 :: Tag)
               in node
            )
            (Node3 Leaf3 Leaf3 (42 :: Int))
    describe "Tree1 (with `InsertFieldSize`)" $ do
        test
            "Leaf"
            (storable (0 :: Tag) <> storable (8 :: Int32) <> storable (10 :: Int))
            (Leaf4 10 :: Tree4 Int)
        test
            "Node"
            ( let node =
                    storable (1 :: Tag)
                        <> packedNodeFieldSize
                        <> leafL
                        <> packedNodeFieldSize
                        <> leafR
                  packedNodeFieldSize = storable (1 + 8 + 8 :: Int32)
                  packedLeafFieldSize = storable (8 :: Int32)
                  leafL = storable (0 :: Tag) <> packedLeafFieldSize <> storable (4 :: Int)
                  leafR = storable (0 :: Tag) <> packedLeafFieldSize <> storable (5 :: Int)
               in node
            )
            (Node4 (Leaf4 4) (Leaf4 5) :: Tree4 Int)
  where
    test name bldr t = it name $ case Data.Packed.unpack (unsafeToPacked (builderBytes bldr)) of
        (tree, _) -> tree `shouldBe` t
