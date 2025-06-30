{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

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

    -- * Applicative
    pure,
    liftA,
    liftA2,
    (<*>),
    (*>),
    (<*),

    -- * Monad
    (>>=),
    (>>),
    lift,
    fail,
    return,
    ReaderPtr,
) where

import Data.ByteString.Internal
import Data.Packed.Packed
import Data.Packed.Utils ((:++:))
import Foreign
import Prelude hiding (fail, liftA2, pure, return, (*>), (<*), (<*>), (>>), (>>=))
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
        IO (v, ReaderPtr r, Int)
    }

{-# INLINE mkPackedReader #-}

-- | Builds a 'PackedReader'
mkPackedReader ::
    ( ReaderPtr (p :++: r) ->
      Int ->
      IO (v, ReaderPtr r, Int)
    ) ->
    PackedReader p r v
mkPackedReader = PackedReader

instance Functor (PackedReader p r) where
    {-# INLINE fmap #-}
    fmap f (PackedReader reader) = PackedReader $ \ptr l -> do
        (!n, !rest, !l1) <- reader ptr l
        Prelude.return (f n, rest, l1)

-------- Applicative

{-# INLINE pure #-}
pure :: v -> PackedReader '[] r v
pure = return

{-# INLINE liftA #-}
liftA :: (a -> b) -> PackedReader p t a -> PackedReader p t b
liftA = fmap

{-# INLINE liftA2 #-}
liftA2 ::
    (v1 -> v2 -> v3) ->
    PackedReader p (r1 :++: r2) v1 ->
    PackedReader r1 r2 v2 ->
    PackedReader (p :++: r1) r2 v3
liftA2 f m1 m2 = PackedReader $ \packed l -> do
    (!v1, !packed1, !l1) <- runPackedReader m1 packed l
    (!v2, !rest, !l2) <- runPackedReader m2 packed1 l1
    Prelude.return (f v1 v2, rest, l2)

{-# INLINE (<*>) #-}
(<*>) :: PackedReader p (r1 :++: r2) (v1 -> v2) -> PackedReader r1 r2 v1 -> PackedReader (p :++: r1) r2 v2
(<*>) = liftA2 id

{-# INLINE (*>) #-}
(*>) :: PackedReader p (r1 :++: r2) v -> PackedReader r1 r2 v' -> PackedReader (p :++: r1) r2 v'
(*>) = (>>)

{-# INLINE (<*) #-}
(<*) :: PackedReader p (r1 :++: r2) v2 -> PackedReader r1 r2 v1 -> PackedReader (p :++: r1) r2 v2
(<*) = liftA2 const

-------- Monad

-- | Allows bindings 'Data.Packed.Reader.PackedReader' together, in a monad-like manner.
--
-- Similar to 'Prelude.>>='
{-# INLINE (>>=) #-}
(>>=) ::
    PackedReader p (r1 :++: r2) v ->
    (v -> PackedReader r1 r2 v') ->
    PackedReader (p :++: r1) r2 v'
(>>=) m1 m2 = PackedReader $ \packed l -> do
    (!value, !packed1, !l1) <- runPackedReader m1 packed l
    (!res, !rest, !l2) <- runPackedReader (m2 value) packed1 l1
    Prelude.return (res, rest, l2)

{-# INLINE (>>) #-}

-- | Similar to 'Prelude.>>'
(>>) ::
    PackedReader p (r1 :++: r2) v ->
    PackedReader r1 r2 v' ->
    PackedReader (p :++: r1) r2 v'
(>>) m1 m2 = PackedReader $ \packed l -> do
    (!_, !packed1, !l1) <- runPackedReader m1 packed l
    runPackedReader m2 packed1 l1

{-# INLINE return #-}

-- | Like 'Prelude.return', wraps a value in a 'PackedReader' that will not consume its input.
return :: v -> PackedReader '[] r v
return value = PackedReader $ \(!packed) !l ->
    Prelude.return (value, packed, l)

{-# INLINE fail #-}
fail :: String -> PackedReader '[] r v
fail msg = mkPackedReader $ \_ _ -> Prelude.fail msg

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
lift r p = mkPackedReader $ \old l -> do
    (!res, _) <- runReader r p
    Prelude.return (res, old, l)

-- | Run the reading function using a ByteString.
{-# INLINE runReader #-}
runReader ::
    PackedReader p r v ->
    Packed (p :++: r) ->
    IO (v, Packed r)
runReader (PackedReader f) (Packed (BS fptr l)) = withForeignPtr fptr $ \ptr -> do
    (!v, !ptr1, !l1) <- f (castPtr ptr) l
    !fptr1 <- newForeignPtr_ ptr1
    Prelude.return (v, Packed (BS fptr1 l1))
