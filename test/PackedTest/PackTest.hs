module PackedTest.PackTest (specs) where

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
specs = describe "Pack Trees" $ do
    describe "Tree1" $ do
        test
            "Leaf"
            (Leaf1 10 :: Tree1 Int)
            (storable (0 :: Tag) <> storable (10 :: Int))

        test
            "Node"
            (Node1 (Node1 (Leaf1 1) (Leaf1 2)) (Leaf1 3) :: Tree1 Int)
            ( let node = storable (1 :: Tag) <> subNode <> leaf
                  subNode = storable (1 :: Tag) <> subLeafL <> subLeafR
                  subLeafL = storable (0 :: Tag) <> storable (1 :: Int)
                  subLeafR = storable (0 :: Tag) <> storable (2 :: Int)
                  leaf = storable (0 :: Tag) <> storable (3 :: Int)
               in node
            )
    describe "Test2" $ do
        test
            "Leaf"
            (Leaf2 :: Tree2 Int)
            (storable (0 :: Tag))
        test
            "Node"
            (Node2 Leaf2 (1 :: Int) Leaf2)
            ( let node = storable (1 :: Tag) <> leaf <> storable (1 :: Int) <> leaf
                  leaf = storable (0 :: Tag)
               in node
            )
    describe "Test3" $ do
        test
            "Leaf"
            (Leaf3 :: Tree3 Int)
            (storable (0 :: Tag))
        test
            "Node"
            (Node3 Leaf3 Leaf3 (42 :: Int))
            ( let node = storable (1 :: Tag) <> leaf <> leaf <> storable (42 :: Int)
                  leaf = storable (0 :: Tag)
               in node
            )
    describe "Tree1 (with `InsertFieldSize`)" $ do
        test
            "Leaf"
            (Leaf4 10 :: Tree4 Int)
            (storable (0 :: Tag) <> storable (8 :: Int32) <> storable (10 :: Int))
        test
            "Node"
            (Node4 (Leaf4 4) (Leaf4 5) :: Tree4 Int)
            ( let node =
                    storable (1 :: Tag)
                        <> packedNodeFieldSize
                        <> leafL
                        <> packedNodeFieldSize
                        <> leafR
                  packedNodeFieldSize = storable (1 + 4 + 8 :: Int32)
                  -- 1 for tag, 4 for field size and 8 for Int
                  packedLeafFieldSize = storable (8 :: Int32)
                  leafL = storable (0 :: Tag) <> packedLeafFieldSize <> storable (4 :: Int)
                  leafR = storable (0 :: Tag) <> packedLeafFieldSize <> storable (5 :: Int)
               in node
            )
  where
    test name tree bldr =
        it name $
            let ptree = Data.Packed.pack tree
             in fromPacked ptree `shouldBe` builderBytes bldr
