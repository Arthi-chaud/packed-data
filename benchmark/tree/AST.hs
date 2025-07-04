{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}

module AST (benchmark) where

import Criterion.Main
import Data.ByteString.Internal
import Data.Packed
import qualified Data.Packed.Reader as R
import Data.Void
import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Utils
import Prelude hiding (sum)

foreign import capi unsafe "benchmark.h eval" c_eval :: Ptr Void -> IO CLong

foreign import capi unsafe "benchmark.h build_ast" c_build_ast :: CInt -> IO (Ptr Void)

foreign import capi unsafe "benchmark.h free_ast" c_free_ast :: Ptr Void -> IO ()

data AST = Value Int32 | Add AST AST | Sub AST AST | Mul AST AST

$(mkPacked ''AST [])

benchmark :: [Int] -> Benchmark
benchmark depths =
    bgroup
        "ast"
        $ fmap buildAndEvaluateASTWithDepth depths

buildAndEvaluateASTWithDepth :: Int -> Benchmark
buildAndEvaluateASTWithDepth n =
    bgroup
        (depthGroupName n)
        [ envWithCleanup (c_build_ast $ fromIntegral n) c_free_ast $ bench cTestName . nfAppIO c_eval
        , bench nativeTestName $ nf eval nativeAST
        , bench packedTestName $ nf (runReader evalPacked) packedAST
        , bench packedWithUnpackTestName $ whnf (eval . fst . unpack) packedAST
        , bench nonMonadicPackedTestName $ nfAppIO evalPackedNonMonadic packedAST
        ]
  where
    !packedAST = pack nativeAST
    !nativeAST = buildNativeAST n

eval :: AST -> Int32
eval (Value n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b

evalPacked :: PackedReader '[AST] r Int32
evalPacked =
    caseAST
        (reader R.>>= \(!n) -> R.return n)
        (opLambda (+))
        (opLambda (-))
        (opLambda (*))
  where
    {-# INLINE opLambda #-}
    opLambda ::
        (Int32 -> Int32 -> Int32) ->
        PackedReader '[AST, AST] r Int32
    opLambda f = R.do
        !left <- evalPacked
        !right <- evalPacked
        let !res = f left right
        R.return res

evalPackedNonMonadic :: Packed (AST ': r) -> IO Int32
evalPackedNonMonadic packed = fst <$> go (unsafeForeignPtrToPtr fptr)
  where
    (BS fptr _) = fromPacked packed
    go :: Ptr Word8 -> IO (Int32, Ptr Word8)
    go ptr = do
        tag <- peek ptr :: IO Word8
        let !nextPtr = ptr `plusPtr` 1
        case tag of
            0 -> do
                !n <- peek nextPtr :: IO Int32
                return (n, plusPtr nextPtr (sizeOf n))
            1 -> opLambda (+) nextPtr
            2 -> opLambda (-) nextPtr
            3 -> opLambda (*) nextPtr
            _ -> undefined
    {-# INLINE opLambda #-}
    opLambda :: (Int32 -> Int32 -> Int32) -> Ptr Int32 -> IO (Int32, Ptr Word8)
    opLambda f ptr = do
        (!left, !r) <- go $ castPtr ptr
        (!right, !r1) <- go r
        let !res = left `f` right
        return (res, r1)

buildNativeAST :: Int -> AST
buildNativeAST 0 = Value 1
buildNativeAST n = Add (buildNativeAST (n - 1)) (buildNativeAST (n - 1))
