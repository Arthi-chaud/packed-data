module Data.Packed (
    -- * Classes
    Packable (..),
    pack,
    Unpackable (..),
    readerWithoutShift,
    unpack,
    unpack',

    -- * Needs
    Needs,
    withEmptyNeeds,
    writeWithFieldSize,
    finish,
    unsafeCastNeeds,

    -- * Packed
    Packed,
    skipWithFieldSize,
    isolate,
    fromPacked,
    unsafeToPacked,
    unsafeCastPacked,

    -- * PackedReader
    PackedReader,
    mkPackedReader,
    runReader,
    readerWithFieldSize,

    -- * Case expression,
    PackedCase (..),

    -- * Code generation
    mkPacked,
    PackingFlag (..),

    -- * Utils
    FieldSize,
    getFieldSizeFromPacked,
    Skippable (..),
) where

import Data.Packed.Case
import Data.Packed.FieldSize
import Data.Packed.Instances ()
import Data.Packed.Needs
import Data.Packed.Packable
import Data.Packed.Packed
import Data.Packed.Reader
import Data.Packed.Skippable
import Data.Packed.TH
import Data.Packed.Unpackable
