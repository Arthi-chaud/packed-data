module Utils (
    toRight,
    cTestName,
    nativeTestName,
    packedTestName,
    packedWithUnpackTestName,
    nonMonadicPackedTestName,
    packedWithFieldSizeTestName,
    nonMonadicPackedWithSizeTestName,
    depthGroupName,
) where

import Text.Printf

toRight :: Either b a -> a
toRight (Right a) = a
toRight _ = error "unexpected left"

cTestName :: String
cTestName = "c"

nativeTestName :: String
nativeTestName = "native"

packedTestName :: String
packedTestName = "packed"

packedWithUnpackTestName :: String
packedWithUnpackTestName = "packed-unpacked"

nonMonadicPackedTestName :: String
nonMonadicPackedTestName = "non-monadic-" ++ packedTestName

packedWithFieldSizeTestName :: String
packedWithFieldSizeTestName = "packed-with-size"

nonMonadicPackedWithSizeTestName :: String
nonMonadicPackedWithSizeTestName = "non-monadic-" ++ packedWithFieldSizeTestName

depthGroupName :: Int -> String
depthGroupName = printf "%02d-depth"
