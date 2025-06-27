{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- It is recommended to import this module like so:
--
-- @
-- import qualified Data.Packed.Reader as R
-- @
module Data.Packed.Reader (
    PackedReader (..),
    mkPackedReader,
    runReader,
    (>>=),
    (>>),
    lift,
    fail,
    return,
    ReaderPtr,
    liftIO,
) where

import Data.ByteString.Internal
import Data.Packed.Internal
import Data.Packed.Packed
import Data.Packed.Utils ((:++:))
import Foreign
import GHC.Exts
import GHC.IO (unIO)
import Prelude hiding (fail, return, (>>), (>>=))
import qualified Prelude

type ReaderPtr r = Ptr Word8

-- | Basically a function that reads/desrialises a value from a 'Data.Packed.Packed'
--
-- 'p' the types of the packed values to read
--
-- 'r' the packed type after the encoded values to read
--
-- 'v' the type of the value to unpack
--
-- __Note:__ It is an indexed monad.
newtype PackedReader p r v = PackedReader
    { runPackedReader ::
        ReaderPtr (p :++: r) ->
        Int ->
        (# v, ReaderPtr r, Int #)
    }

{-# INLINE mkPackedReader #-}

-- | Builds a 'PackedReader'
mkPackedReader ::
    ( ReaderPtr (p :++: r) ->
      Int ->
      (# v, ReaderPtr r, Int #)
    ) ->
    PackedReader p r v
mkPackedReader = PackedReader

instance Functor (PackedReader p r) where
    {-# INLINE fmap #-}
    fmap f (PackedReader reader) = PackedReader $ \ptr l ->
        let !(# !n, !rest, !l1 #) = reader ptr l
         in (# f n, rest, l1 #)

{-# INLINE (>>=) #-}

-- | Allows bindings 'Data.Packed.Reader.PackedReader' together, in a monad-like manner.
--
-- Similar to 'Prelude.>>='
(>>=) ::
    PackedReader p (r1 :++: r2) v ->
    (v -> PackedReader r1 r2 v') ->
    PackedReader (p :++: r1) r2 v'
(>>=) m1 m2 = PackedReader $ \packed l ->
    let
        !(# !value, !packed1, !l1 #) = runPackedReader m1 packed l
        !(# !res, !rest, !l2 #) = runPackedReader (m2 value) packed1 l1
     in
        (# res, rest, l2 #)

{-# INLINE (>>) #-}

-- | Similar to 'Prelude.>>'
(>>) ::
    PackedReader p (r1 :++: r2) v ->
    PackedReader r1 r2 v' ->
    PackedReader (p :++: r1) r2 v'
(>>) m1 m2 = PackedReader $ \packed l ->
    let
        !(# !_, !packed1, !l1 #) = runPackedReader m1 packed l
     in
        runPackedReader m2 packed1 l1

{-# INLINE return #-}

-- | Like 'Prelude.return', wraps a value in a 'PackedReader' that will not consume its input.
return :: v -> PackedReader '[] r v
return value = PackedReader $ \(!packed) !l -> (# value, packed, l #)

{-# INLINE fail #-}
fail :: String -> PackedReader '[] r v
fail msg = mkPackedReader $ \_ _ -> error msg

-- | Allows reading another packed value in a do-notation.
--
-- The reading of the second stream does not consume anything from the first.
--
-- __Example__:
--
-- @
-- import qualified Data.Packed.Reader as R
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
--
-- packedTreeToList :: 'PackedReader' '[Tree Int] '[] [Int]
-- packedTreeToList = go []
--     where
--         go l =
--             caseTree
--                 (R.return l)
--                 ( R.do
--                     packedLeft <- 'Data.Packed.isolate'
--                     n <- 'Data.Packed.readerWithFieldSize'
--                     packedRight <- 'Data.Packed.isolate'
--                     -- Using lift allows consuming the packedRight value
--                     rightList <- R.'lift' (go l) packedRight
--                     R.'lift' (go $ n : rightList) packedLeft
--                 )
-- @
{-# INLINE lift #-}
lift ::
    PackedReader a b v ->
    Packed (a :++: b) ->
    PackedReader '[] r v
lift r p = mkPackedReader $ \old l ->
    let
        !(!res, _) = runReader r p
     in
        (# res, old, l #)

{-# INLINE liftIO #-}
liftIO :: IO a -> PackedReader b c a
liftIO io = mkPackedReader $ \ptr size -> case runRW# (unIO io) of
    (# _, !a #) -> (# a, ptr, size #)

-- | Run the reading function using a ByteString.
{-# INLINE runReader #-}
runReader ::
    PackedReader p r v ->
    Packed (p :++: r) ->
    (v, Packed r)
runReader (PackedReader f) (Packed (BS fptr l)) =
    unsafeDupablePerformIO
        ( withForeignPtr fptr $ \ptr -> do
            let (# !v, !ptr1, !l1 #) = f (castPtr ptr) l
            !fptr1 <- newForeignPtr_ ptr1
            Prelude.return (v, Packed (BS fptr1 l1))
        )
