{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}

module Traversals (benchmark) where

import Criterion.Main
import Data.ByteString.Internal
import Data.Packed
import qualified Data.Packed.Reader as R
import Data.Void
import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Utils

foreign import capi unsafe "benchmark.h get_right_most" c_get_right_most :: Ptr Void -> IO CLong

foreign import capi unsafe "benchmark.h build_tree" c_build_tree :: CInt -> IO (Ptr Void)

foreign import capi unsafe "benchmark.h free_tree" c_free_tree :: Ptr Void -> IO ()

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

$(mkPacked ''Tree1 [])
$(mkPacked ''Tree2 [InsertFieldSize])

benchmark :: [Int] -> Benchmark
benchmark depths =
    bgroup
        "traversals"
        [ bgroup
            "right-most-node"
            $ fmap compareGettingRightMostNode depths
        , bgroup
            "contains"
            $ fmap compareContainsValue depths
        ]

compareGettingRightMostNode :: Int -> Benchmark
compareGettingRightMostNode n =
    bgroup
        (depthGroupName n)
        [ envWithCleanup (c_build_tree (fromIntegral n)) c_free_tree $ bench cTestName . nfAppIO c_get_right_most
        , bench nativeTestName $ nf getRightMostNodeNative nativeTree
        , bench packedTestName $ nfAppIO (runReader getRightMostNodePacked) packedTree
        , bench packedWithUnpackTestName $ whnf (getRightMostNodeNative . fst . unpack) packedTree
        , bench nonMonadicPackedTestName $ nfAppIO getRightMostNodePackedNonMonadic packedTree
        , bench packedWithFieldSizeTestName $
            nfAppIO (runReader getRightMostNodePacked2) packedTreeWithSize
        , bench nonMonadicPackedWithSizeTestName $ nfAppIO getRightMostNodePacked2NonMonadic packedTreeWithSize
        ]
  where
    packedTree = pack nativeTree
    packedTreeWithSize = pack (tree1ToTree2 nativeTree)
    tree1ToTree2 (Leaf1 l) = Leaf2 l
    tree1ToTree2 (Node1 a b) = Node2 (tree1ToTree2 a) (tree1ToTree2 b)
    nativeTree = buildNativeTree n

getRightMostNodeNative :: Tree1 Int -> Int
getRightMostNodeNative (Leaf1 n) = n
getRightMostNodeNative (Node1 _ r) = getRightMostNodeNative r

getRightMostNodePacked :: PackedReader '[Tree1 Int] r Int
getRightMostNodePacked =
    caseTree1
        reader
        (skip R.>> getRightMostNodePacked)

getRightMostNodePackedNonMonadic :: Packed (Tree1 Int ': r) -> IO Int
getRightMostNodePackedNonMonadic packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Int, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        case tag of
            0 -> do
                !n <- peek (plusPtr ptr 1)
                return (n, plusPtr ptr 9)
            1 -> do
                (!_, !r) <- go (plusPtr ptr 1)
                (!right, !r1) <- go r
                return (right, r1)
            _ -> undefined

getRightMostNodePacked2 :: PackedReader '[Tree2 Int] '[] Int
getRightMostNodePacked2 =
    caseTree2
        readerWithFieldSize
        ( R.do
            skipWithFieldSize
            skip R.>> getRightMostNodePacked2
        )

getRightMostNodePacked2NonMonadic :: Packed (Tree2 Int ': r) -> IO Int
getRightMostNodePacked2NonMonadic packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    sizeOfFieldSize = sizeOf (1 :: Int32)
    sizeOfInt = sizeOf (1 :: Int)
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Int, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        let nextPtr = ptr `plusPtr` 1
        case tag of
            0 -> do
                !n <- peek (plusPtr nextPtr sizeOfFieldSize)
                let nextPtr1 = nextPtr `plusPtr` (sizeOfFieldSize + sizeOfInt)
                return (n, nextPtr1)
            1 -> do
                !fieldSize <- peek (castPtr nextPtr :: Ptr Int32)
                let nextPtr1 = nextPtr `plusPtr` (sizeOfFieldSize + fromIntegral fieldSize) `plusPtr` sizeOfFieldSize
                (!right, !r1) <- go nextPtr1
                return (right, r1)
            _ -> undefined

buildNativeTree :: Int -> Tree1 Int
buildNativeTree 0 = Leaf1 0
buildNativeTree n = Node1 subTree subTree
  where
    subTree = buildNativeTree (n - 1)

-- Contains

compareContainsValue :: Int -> Benchmark
compareContainsValue n =
    bgroup
        (depthGroupName n)
        [ bench nativeTestName $ nf (containsNative value) nativeTree
        , bench packedTestName $ nfAppIO (runReader (containsPacked value)) packedTree
        , bench nonMonadicPackedTestName $ nfAppIO (containsNonMonadic value) packedTree
        , bench packedWithFieldSizeTestName $
            nfAppIO (runReader (containsPacked2 value)) packedTreeWithSize
        , bench nonMonadicPackedWithSizeTestName $ nfAppIO (containsNonMonadic2 value) packedTreeWithSize
        ]
  where
    packedTree = pack nativeTree
    packedTreeWithSize = pack (tree1ToTree2 nativeTree)
    tree1ToTree2 (Leaf1 l) = Leaf2 l
    tree1ToTree2 (Node1 a b) = Node2 (tree1ToTree2 a) (tree1ToTree2 b)
    nativeTree = buildNativeTreeContains n value
    value = 500

containsNative :: Int -> Tree1 Int -> Bool
containsNative value (Leaf1 n) = n == value
containsNative value (Node1 s1 s2) = containsNative value s1 || containsNative value s2

containsPacked :: Int -> PackedReader '[Tree1 Int] r Bool
containsPacked n =
    caseTree1
        ( R.do
            i <- reader
            R.return (i == n)
        )
        ( R.do
            s1 <- containsPacked n
            if s1 then skip R.>> R.return s1 else containsPacked n
        )

containsPacked2 :: Int -> PackedReader '[Tree2 Int] r Bool
containsPacked2 n =
    caseTree2
        ( R.do
            i <- readerWithFieldSize
            R.return (i == n)
        )
        ( R.do
            s1 <- skip R.>> containsPacked2 n
            if s1 then skipWithFieldSize R.>> R.return s1 else skip R.>> containsPacked2 n
        )

containsNonMonadic :: Int -> Packed (Tree1 Int ': r) -> IO Bool
containsNonMonadic n packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Bool, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        case tag of
            0 -> do
                !i <- peek (plusPtr ptr 1)
                return (i == n, plusPtr ptr 9)
            1 -> do
                (!b, !r) <- go (plusPtr ptr 1)
                if b
                    then do
                        (!_, !r1) <- go r
                        return (True, r1)
                    else do
                        (!right, !r1) <- go r
                        return (right, r1)
            _ -> undefined

containsNonMonadic2 :: Int -> Packed (Tree2 Int ': r) -> IO Bool
containsNonMonadic2 n packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    sizeOfFieldSize = sizeOf (1 :: Int32)
    sizeOfInt = sizeOf (1 :: Int)
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Bool, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        let nextPtr = ptr `plusPtr` 1
        case tag of
            0 -> do
                !i <- peek (plusPtr nextPtr sizeOfFieldSize)
                let nextPtr1 = nextPtr `plusPtr` (sizeOfFieldSize + sizeOfInt)
                return (i == n, nextPtr1)
            1 -> do
                let subTree1Ptr = nextPtr `plusPtr` sizeOfFieldSize
                (!b, !nextPtr1) <- go subTree1Ptr
                if b
                    then do
                        !fieldSize <- peek (castPtr nextPtr1 :: Ptr Int32)
                        return (True, castPtr $ nextPtr1 `plusPtr` (sizeOfFieldSize + fromIntegral fieldSize))
                    else go (nextPtr1 `plusPtr` sizeOfFieldSize)
            _ -> undefined

buildNativeTreeContains :: Int -> Int -> Tree1 Int
buildNativeTreeContains 0 _ = Leaf1 0
buildNativeTreeContains 1 n = Node1 (Leaf1 n) (buildNativeTreeContains 0 n)
buildNativeTreeContains depth n = Node1 subTree subTree
  where
    subTree = buildNativeTreeContains (depth - 1) n
