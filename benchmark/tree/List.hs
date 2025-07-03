{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module List (benchmark) where

import Control.DeepSeq
import Criterion.Main
import Data.Packed
import qualified Data.Packed.Reader as R
import Foreign
import Prelude hiding (sum)

data Tree1 a = Leaf1 | Node1 (Tree1 a) a (Tree1 a)

instance NFData (Tree1 a) where
    rnf Leaf1 = ()
    rnf (Node1 l n r) = n `seq` l `seq` r `seq` ()

$(mkPacked ''Tree1 [InsertFieldSize])

benchmark :: [Int] -> Benchmark
benchmark _ =
    bgroup
        "list"
        []

-- bgroup
--     "from-list"
--     $ fmap buildTreeFromListWithLength depths
-- , bgroup
--     "to-list"
--     $ fmap treeToListWithLength depths

-- buildTreeFromListWithLength :: Int -> Benchmark
-- buildTreeFromListWithLength n =
--     bgroup
--         (depthGroupName n)
--         [ bench nativeTestName $ nf buildTreeFromList list
--         , bench packedTestName $ nfAppIO buildPackedTreeFromList list
--         ]
--   where
--     list = buildUnorderedList n

-- treeToListWithLength :: Int -> Benchmark
-- treeToListWithLength n =
--     bgroup
--         (depthGroupName n)
--         [ bench nativeTestName $ nf treeToList (buildTreeFromList list)
--         , env (buildPackedTreeFromList list) $
--             bench packedTestName
--                 . nfAppIO
--                     (runReader packedTreeToList)
--         ]
-- where
--   list = buildUnorderedList n

buildTreeFromList :: [Int] -> Tree1 Int
buildTreeFromList = foldl (flip insertInTree) Leaf1
  where
    insertInTree n Leaf1 = Node1 Leaf1 n Leaf1
    insertInTree n (Node1 l n' r) =
        if n > n'
            then Node1 l n' (insertInTree n r)
            else Node1 (insertInTree n l) n' r

treeToList :: Tree1 Int -> [Int]
treeToList t = go t []
  where
    go Leaf1 r = r
    go (Node1 l n r) list = go l (n : go r list)

packedTreeToList :: PackedReader '[Tree1 Int] '[] [Int]
packedTreeToList = go []
  where
    go :: [Int] -> PackedReader '[Tree1 Int] r [Int]
    go l =
        caseTree1
            (R.return l)
            ( R.do
                packedLeft <- isolate
                n <- readerWithFieldSize
                packedRight <- isolate
                rightList <- R.lift (go l) packedRight
                R.lift (go $ n : rightList) packedLeft
            )

--
-- buildPackedTreeFromList :: [Int] -> IO (Packed '[Tree1 Int])
-- buildPackedTreeFromList l =
--     finish
--         =<< withEmptyNeeds
--         =<< foldl
--             ( \builder i -> R.do
--                 n <- insertInPackedTree i
--                 R.return (builder N.>> n)
--             )
--             (write Leaf1)
--             l
--   where
--     packedToNeeds :: Packed a -> Needs '[] a
--     packedToNeeds p = let (BS.BS fptr l) = fromPacked p in Needs fptr l l
--     insertInPackedTree ::
--         Int ->
--         PackedReader '[Tree1 Int] '[] (N.NeedsBuilder '[] '[Tree1 Int] '[] '[Tree1 Int])
--     insertInPackedTree n =
--         caseTree1
--             ( mkPackedReader
--                 ( \p l -> do
--                     let n =
--                             ( N.do
--                                 startNode1
--                                 writeWithFieldSize Leaf1
--                                 writeWithFieldSize 0
--                                 writeWithFieldSize Leaf1
--                             )
--                     return (n, p, l)
--                 )
--             )
--             ( R.do
--                 !left <- isolate
--                 !n' <- readerWithFieldSize
--                 !right <- isolate
--                 let !needLeft = packedToNeeds left
--                 let !needRight = packedToNeeds right
--                 needsN' <- mkPackedReader (\p l -> (,p,l) <$> withEmptyNeeds (write n'))
--                 if n > n'
--                     then R.do
--                         right' <- R.lift (insertInPackedTree n) right
--                         R.return (repackNode1 needLeft needsN' right')
--                     else R.do
--                         left' <- R.lift (insertInPackedTree n) left
--                         R.return (repackNode1 left' needsN' needRight)
--             )
--
buildUnorderedList :: Int -> [Int]
buildUnorderedList n = intercalate [(n `div` 2) .. n] [0 .. (n `div` 2) - 1]
  where
    intercalate a [] = a
    intercalate [] b = b
    intercalate (a : b) (c : d) = a : c : intercalate b d
