{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tree (Tree (..), caseTree, readTree) where

import Data.Packed

data Tree a = Leaf a | Node (Tree a) (Tree a)

$(mkPacked ''Tree [InsertFieldSize, SkipLastFieldSize])
