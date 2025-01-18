{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Packed.Needs (
    -- * Type
    Needs (..),
    withEmptyNeeds,
    finish,

    -- * Builders
    NeedsBuilder (..),
    NeedsWriter,
    NeedsWriter',
    (>>),
    mkNeedsBuilder,
    withNeeds,

    -- * Mixing Needs together
    concatNeeds,
    applyNeeds,

    -- * Internal
    unsafeCastNeeds,
    (:++:),
) where

import ByteString.StrictBuilder
import Data.Kind
import Data.Packed.Packed
import Data.Packed.Utils
import Prelude hiding ((>>))

-- | A buffer where packed values can be written
-- The order to write these values is defined by the 'l' type list
--
-- If 'p' is an empty list, then a value of type 't' can be extracted from that buffer.
-- (See 'finish')
newtype Needs (p :: [Type]) (t :: [Type]) = Needs Builder

unsafeCastNeeds :: Needs a b -> Needs c d
unsafeCastNeeds (Needs b) = Needs b

-- | A wrapper around a function that builds a 'Needs'
--
-- 'ps': The type of the expected input of the source 'Needs'
--
-- 'ts': The type of the final packed data of the source 'Needs'
--
-- 'pd': The type of the expected input of the resuling 'Needs'
--
-- 'td': The type of the final packed data of the resulting 'Needs'
--
-- __Note:__ It is an indexed monad.
newtype NeedsBuilder ps ts pd td = NeedsBuilder
    { runBuilder :: Needs ps ts -> Needs pd td
    }

(>>) :: NeedsBuilder p1 t1 p2 t2 -> NeedsBuilder p2 t2 p3 t3 -> NeedsBuilder p1 t1 p3 t3
(NeedsBuilder !b1) >> (NeedsBuilder !b2) = mkNeedsBuilder (\n -> b2 (b1 n))

-- | Shortcut type for 'NeedsBuilder'\'s that simply write a value to a 'Needs' without changing the final packed type
type NeedsWriter a r t = NeedsBuilder (a ': r) t r t

-- | Shortcut type for 'NeedsBuilder'\'s that simply write multiple values to a 'Needs' without changing the final packed type
type NeedsWriter' a r t = NeedsBuilder (a :++: r) t r t

mkNeedsBuilder :: (Needs ps ts -> Needs pd td) -> NeedsBuilder ps ts pd td
mkNeedsBuilder = NeedsBuilder

withEmptyNeeds :: NeedsBuilder a b x y -> Needs x y
withEmptyNeeds (NeedsBuilder !b) = b (Needs mempty)

withNeeds :: Needs x y -> NeedsBuilder x y x1 y1 -> Needs x1 y1
withNeeds needs (NeedsBuilder !next) = next needs

concatNeeds :: Needs p t -> NeedsBuilder '[] t1 p (t1 :++: t)
concatNeeds (Needs !b) = NeedsBuilder (\(Needs s) -> Needs (s <> b))

applyNeeds :: Needs '[] t1 -> NeedsBuilder (t1 :++: r) t r t
applyNeeds (Needs b) = NeedsBuilder (\(Needs s) -> Needs (s <> b))

-- | Turns a 'Needs' value (that does not expect to be written to) to a 'Data.Packed.Packed'
finish :: Needs '[] t -> Packed t
finish (Needs !b) = Packed (builderBytes b)
