{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Criterion.Main (bench, defaultMain, nf, nfAppIO)
import Data.Packed
import qualified Data.Packed.Reader as R

data Arithmetic where
    Val :: Double -> Arithmetic
    Add :: Arithmetic -> Arithmetic -> Arithmetic
    Sub :: Arithmetic -> Arithmetic -> Arithmetic
    Mul :: Arithmetic -> Arithmetic -> Arithmetic
    Div :: Arithmetic -> Arithmetic -> Arithmetic

$(mkPacked ''Arithmetic [])

eval :: Arithmetic -> Double
eval (Val d) = d
eval (Add a b) = (eval a) + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b

evalPacked :: PackedReader '[Arithmetic] r Double
evalPacked =
    caseArithmetic
        reader
        (fn (+))
        (fn (-))
        (fn (*))
        (fn (/))
  where
    {-# INLINE fn #-}
    fn op = R.do
        !l <- evalPacked
        !r <- evalPacked
        R.return (l `op` r)

main :: IO ()
main = do
    let bigAst = genBigAst
        packedAst = pack bigAst
    defaultMain
        [ bench "packed" $ nfAppIO (runReader evalPacked) packedAst
        , bench "unpacked" $ nf eval bigAst
        ]

genBigAst :: Arithmetic
genBigAst = go 27
  where
    go :: Int -> Arithmetic
    go 0 = Val 3
    go n
        | n `mod` 5 == 0 = Div child child
        | n `mod` 4 == 0 = Mul child child
        | n `mod` 3 == 0 = Sub child child
        | otherwise = Add child child
      where
        child = go (n - 1)
