module PackedTest.Data (
    Tree1 (..),
    Tree2 (..),
    Tree3 (..),
    Tree4 (..),
    MyData (..),
) where

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving (Show, Eq)

data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) deriving (Show, Eq)

data Tree3 a = Leaf3 | Node3 (Tree3 a) (Tree3 a) a deriving (Show, Eq)

-- | Basically the same as Tree1, but could be used to test flags
data Tree4 a = Leaf4 a | Node4 (Tree4 a) (Tree4 a) deriving (Show, Eq)

data MyData = SmallData Int | BigData String deriving (Eq, Show)
