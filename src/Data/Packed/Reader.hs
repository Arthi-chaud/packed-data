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
    fail,
    return,
    lift,

    -- * Application
    with,
    threadedWith,

    -- * Fragments
    PackedFragment (..),
    castPackedFragment,
) where

import Control.Monad.Identity
import Data.Bifunctor
import Data.ByteString.Internal
import Data.Kind
import Data.Packed.Internal
import Data.Packed.Packed
import Data.Packed.Utils ((:++:))
import Foreign hiding (with)
import Prelude hiding (fail, return, (>>), (>>=))
import qualified Prelude

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
    { runReaderStep :: PackedFragment (p :++: r) -> Identity (v, PackedFragment r)
    }

-- | Represents position of the cursor in the packed buffer to traverse
data PackedFragment (p :: [Type])
    = PF
        -- | Pointer in the packed buffer
        {-# UNPACK #-} !(Ptr Word8)
        -- | Number of bytes between the cursor's position and the end of the buffer
        {-# UNPACK #-} !Int

{-# INLINE castPackedFragment #-}
castPackedFragment :: PackedFragment p -> PackedFragment t
castPackedFragment (PF p t) = PF p t

{-# INLINE mkPackedReader #-}

-- | Builds a 'Data.Packed.Reader.PackedReader'
mkPackedReader ::
    (PackedFragment (p :++: r) -> Identity (v, PackedFragment r)) ->
    PackedReader p r v
mkPackedReader = PackedReader

instance Functor (PackedReader p r) where
    {-# INLINE fmap #-}
    fmap f (PackedReader reader) = PackedReader $ fmap (first f) . reader

{-# INLINE (>>=) #-}

-- | Allows bindings 'Data.Packed.Reader.PackedReader' together, in a monad-like manner.
--
-- Similar to 'Prelude.>>='
(>>=) ::
    PackedReader p (r1 :++: r2) v ->
    (v -> PackedReader r1 r2 v') ->
    PackedReader (p :++: r1) r2 v'
(>>=) pr1 next = PackedReader $ \pf ->
    let Identity (!v1, !pf1) = runReaderStep pr1 (castPackedFragment pf)
        PackedReader !pr2 = next v1
     in pr2 pf1

{-# INLINE (>>) #-}

-- | Similar to 'Prelude.>>'
(>>) ::
    PackedReader p (r1 :++: r2) v ->
    PackedReader r1 r2 v' ->
    PackedReader (p :++: r1) r2 v'
(>>) pr1 (PackedReader pr2) = PackedReader $ \pf ->
    let
        Identity (!_, !pf1) = runReaderStep pr1 (castPackedFragment pf)
     in
        pr2 pf1

{-# INLINE return #-}

-- | Like 'Prelude.return', wraps a value in a 'Data.Packed.Reader.PackedReader' that will not consume its input.
return :: v -> PackedReader '[] r v
return !value = PackedReader $ \(!pf) -> Identity (value, pf)

-- | Interrupts a 'Data.Packed.Reader.PackedReader' computation
{-# INLINE fail #-}
fail :: String -> PackedReader '[] r v
fail msg = mkPackedReader $ \_ -> error msg

-- | Run the reading function using a 'Data.Packed.Packed'.
{-# INLINE runReader #-}
runReader ::
    PackedReader p r v ->
    Packed (p :++: r) ->
    (v, Packed r)
runReader pr (Packed (BS fptr l)) =
    unsafeDupablePerformIO
        ( withForeignPtr fptr $ \ptr -> do
            let Identity (!v, !(PF ptr1 l1)) = runReaderStep pr (PF (castPtr ptr) l)
            !fptr1 <- newForeignPtr_ ptr1
            Prelude.return (v, Packed (BS fptr1 l1))
        )

-- | Consumes the input 'Data.Packed.Reader.PackedFragment'.
--
-- To be used inside a 'Data.Packed.Reader.PackedReader' computation.
--
-- Example:
--
-- @
-- sumPacked :: PackedReader '[Tree Int] r Int
-- sumPacked = PackedReader $ \case
--     PackedLeaf l -> reader `with` l
--     PackedNode n -> threadedWith n $ R.do
--         !left <- sumPacked2
--         !right <- sumPacked2
--         let !res = left + right
--         R.return res
-- @
{-# INLINE with #-}
with :: PackedReader p r v -> PackedFragment (p :++: r) -> Identity (v, PackedFragment r)
with = runReaderStep

-- | Flipped version of 'with'
{-# INLINE threadedWith #-}
threadedWith :: PackedFragment (p :++: r) -> PackedReader p r v -> Identity (v, PackedFragment r)
threadedWith = flip with

-- | Allows running a 'Data.Packed.Reader.PackedReader' computation inside an other
{-# INLINE lift #-}
lift ::
    PackedReader a b v ->
    PackedFragment (a :++: b) ->
    PackedReader '[] r v
lift r p = mkPackedReader $ \pf ->
    let
        Identity !(!res, !_) = runReaderStep r p
     in
        Identity (res, pf)
