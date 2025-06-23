module PackedTest.CaseTest (specs) where

import Data.Int
import Data.Packed
import qualified Data.Packed.Reader as R
import PackedTest.Data
import Test.Hspec

$(mkPacked ''Tree1 [])

specs :: Spec
specs = describe "Case on Trees" $ do
    it "should get the sum of the values in the tree" $ do
        tree <- pack $ buildTree (10 :: Int64)
        let
            computeSum :: PackedReader '[Tree1 Int64] r Int64
            computeSum =
                caseTree1
                    reader
                    ( R.do
                        leftSum <- computeSum
                        rightSum <- computeSum
                        R.return $ leftSum + rightSum
                    )
        (res, _) <- runReader computeSum tree
        res `shouldBe` 55
  where
    buildTree 0 = Leaf1 0
    buildTree n = if odd n then Node1 (buildTree (n - 1)) (Leaf1 n) else Node1 (Leaf1 n) (buildTree (n - 1))
