{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}

module Sum (benchmark) where

import Criterion.Main
import Data.ByteString.Internal
import Data.Packed
import qualified Data.Packed.Reader as R
import Data.Void (Void)
import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import GHC.IO
import Utils
import Prelude hiding (sum)

foreign import capi unsafe "benchmark.h sum" c_sum :: Ptr Void -> IO CLong

foreign import capi unsafe "benchmark.h build_tree" c_build_tree :: CInt -> IO (Ptr Void)

foreign import capi unsafe "benchmark.h free_tree" c_free_tree :: Ptr Void -> IO ()

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

$(mkPacked ''Tree1 [])
$(mkPacked ''Tree2 [InsertFieldSize])

benchmark :: [Int] -> Benchmark
benchmark depths =
    bgroup
        "sum"
        $ fmap computeTreeSumWithDepth depths

computeTreeSumWithDepth :: Int -> Benchmark
computeTreeSumWithDepth n =
    bgroup
        (depthGroupName n)
        [ envWithCleanup (c_build_tree (fromIntegral n)) c_free_tree $ bench cTestName . nfAppIO c_sum
        , bench nativeTestName $ nf sum nativeTree
        , bench packedTestName $ whnfAppIO (R.runReader sumPacked) packedTree
        , bench packedWithUnpackTestName $ whnf (sum . fst . unpack) packedTree
        , bench packedWithFieldSizeTestName $ whnfAppIO (R.runReader sumPacked2) packedTree2
        , bench nonMonadicPackedTestName $ nfAppIO sumPackedNonMonadic packedTree
        , bench nonMonadicPackedWithSizeTestName $ nfAppIO sumPackedNonMonadic2 packedTree2
        ]
  where
    !packedTree = pack nativeTree
    !packedTree2 = pack (buildNativeTree2 n)
    !nativeTree = buildNativeTree n

sum :: Tree1 Int -> Int
sum (Leaf1 n) = n
sum (Node1 t1 t2) = sum t1 + sum t2

sumPacked :: PackedReader '[Tree1 Int] r Int
sumPacked =
    caseTree1
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

sumPacked2 :: PackedReader '[Tree2 Int] r Int
sumPacked2 =
    caseTree2
        ( R.do
            !n <- readerWithFieldSize
            R.return n
        )
        ( R.do
            !left <- skip R.>> sumPacked2
            !right <- skip R.>> sumPacked2
            let !res = left + right
            R.return res
        )
sumPackedNonMonadic :: Packed (Tree1 Int ': r) -> IO Int
sumPackedNonMonadic packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Int, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        let !nextPtr = ptr `plusPtr` 1
        case tag of
            0 -> do
                !n <- peek nextPtr
                let !nextPtr1 = ptr `plusPtr` 9
                return (n, nextPtr1)
            1 -> do
                (!left, !r) <- go (castPtr nextPtr)
                (!right, !r1) <- go r
                let !res = left + right
                return (res, r1)
            _ -> undefined

sumPackedNonMonadic2 :: Packed (Tree2 Int ': r) -> IO Int
sumPackedNonMonadic2 packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    sizeOfFieldSize = sizeOf (1 :: Int32)
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Int, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        let !nextPtr = ptr `plusPtr` 1
        case tag of
            0 -> do
                !n <- peek (nextPtr `plusPtr` sizeOfFieldSize)
                let !nextPtr1 = ptr `plusPtr` 9 `plusPtr` sizeOfFieldSize
                return (n, nextPtr1)
            1 -> do
                (!left, !r) <- go (castPtr $ nextPtr `plusPtr` sizeOfFieldSize)
                (!right, !r1) <- go (r `plusPtr` sizeOfFieldSize)
                let !res = left + right
                return (res, r1)
            _ -> undefined
buildNativeTree :: Int -> Tree1 Int
buildNativeTree 0 = Leaf1 1
buildNativeTree n = Node1 subTree subTree
  where
    subTree = buildNativeTree (n - 1)

buildNativeTree2 :: Int -> Tree2 Int
buildNativeTree2 0 = Leaf2 1
buildNativeTree2 n = Node2 subTree subTree
  where
    subTree = buildNativeTree2 (n - 1)
