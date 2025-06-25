{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Packed.Needs (
    -- * Type
    Needs (..),
    finish,

    -- * Builders
    NeedsBuilder,
    runBuilder,
    (>>=),
    (>=>),
    NeedsWriter,
    NeedsWriter',

    -- * Mixing Needs together
    concatNeeds,
    applyNeeds,

    -- * Internal
    getCursor,
    getOffset,
    getOrigin,
    writeStorable,

    -- * Dangerous
    unsafeWriteTag,
    unsafeCastNeeds,
    guardRealloc,
    unsafeShiftNeedsPtr,
) where

import qualified Control.Functor.Linear as L
import Data.ByteString.Internal
import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.Kind
import qualified Data.Num.Linear as L
import qualified Data.Ord.Linear as L
import Data.Packed.Internal
import Data.Packed.Packed
import Data.Packed.Utils ((:++:))
import Foreign (Storable (..))
import GHC.Exts
import GHC.ForeignPtr
import GHC.IO (IO (..))
import qualified System.IO.Linear as L
import Unsafe.Linear
import Prelude hiding ((>>=))
import Data.Unrestricted.Linear
import GHC.IO.Unsafe (unsafeDupablePerformIO)

data Needs (p :: [Type]) (t :: [Type])
    = Needs
        {-# UNPACK #-} !(MutableByteArray# RealWorld)
        {-# UNPACK #-} !Addr#
        {-# UNPACK #-} !Int

{-# INLINE getCursor #-}
getCursor :: Needs a b %1 -> (# Addr#, Needs a b #)
getCursor = toLinear (\n@(Needs _ cursor _) -> (# cursor, n #))

{-# INLINE getOffset #-}
getOffset :: Needs a b %1 -> (# Int#, Needs a b #)
getOffset = toLinear (\n@(Needs origin cursor _) -> (# cursor `minusAddr#` mutableByteArrayContents# origin, n #))

{-# INLINE getOrigin #-}
getOrigin :: Needs a b %1 -> (# Addr#, Needs a b #)
getOrigin = toLinear (\n@(Needs origin _ _) -> (# mutableByteArrayContents# origin, n #))

{-# INLINE getSpaceLeft #-}
getSpaceLeft :: Needs a b %1 -> (# Int, Needs a b #)
getSpaceLeft = toLinear (\n@(Needs _ _ spaceLeft) -> (# spaceLeft, n #))

{-# INLINE unsafeCastNeeds #-}
unsafeCastNeeds :: Needs a b %1 -> Needs c d
unsafeCastNeeds (Needs a b c) = Needs a b c

--- Needs Builder

type NeedsBuilder p1 t1 p2 t2 = Needs p1 t1 %1 -> L.IO (Needs p2 t2)

{-# INLINE (>>=) #-}
(>>=) :: L.IO (Needs p1 t1) %1 -> (Needs p1 t1 %1 -> L.IO (Needs p2 t2)) -> L.IO (Needs p2 t2)
(>>=) a b = L.do
    !x <- a
    !x1 <- b x
    L.return x1

{-# INLINE (>=>) #-}
(>=>) :: (a %1 -> L.IO t) %1 -> (t %1 -> L.IO b) -> a %1 -> L.IO b
(>=>) a b !c = L.do
    !x <- a c
    !x1 <- b x
    L.return x1

--- | Shortcut type for 'NeedsBuilder'\'s that simply write a value to a 'Needs' without changing the final packed type
type NeedsWriter a r t = NeedsBuilder (a ': r) t r t

--- | Shortcut type for 'NeedsBuilder'\'s that simply write multiple values to a 'Needs' without changing the final packed type
type NeedsWriter' a r t = NeedsBuilder (a :++: r) t r t

baseBufferSize :: Int
baseBufferSize = 1000

{-# INLINE runBuilder #-}
runBuilder :: NeedsBuilder p1 r '[] r ->  Packed r
runBuilder builder = unsafeDupablePerformIO $ do
    srcNeeds <- IO $ \s -> case newAlignedPinnedByteArray# (unInt baseBufferSize) 0# s of
        (# s', byteArray #) -> (# s', Needs byteArray (mutableByteArrayContents# byteArray) baseBufferSize #)
    !finalNeeds <- L.withLinearIO $ L.fmap (toLinear Ur) (builder srcNeeds)
    finish finalNeeds

{-# INLINE finish #-}
finish :: Needs '[] a -> IO (Packed a)
finish (Needs og cursor _) = do
    let !contentLen = cursor `minusAddr#` mutableByteArrayContents# og
    () <- IO $ \s -> case shrinkMutableByteArray# og contentLen s of
        s' -> (# s', () #)
    -- SRC: https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.ForeignPtr.html#mallocPlainForeignPtrBytes
    let !fptr = ForeignPtr (mutableByteArrayContents# og) (PlainPtr og)
    -- BS calls this function when alloacting an ForeignPtr, let's do that too
    -- https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/src/Data.ByteString.Internal.Type.html#mkDeferredByteString
    !bs <- mkDeferredByteString fptr (I# contentLen)
    return $! unsafeToPacked bs

{-# INLINE concatNeeds #-}
concatNeeds :: Needs p t %1 -> NeedsBuilder '[] t1 p (t1 :++: t)
concatNeeds = toLinear2 appendNeeds 

{-# INLINE applyNeeds #-}
applyNeeds :: Needs '[] t1 %1 -> NeedsBuilder (t1 :++: r) t r t
applyNeeds = toLinear2 appendNeeds 

{-# INLINE writeStorable #-}
writeStorable :: (Storable a) => a -> NeedsBuilder (a ': r) t r t
writeStorable !a !needs = L.do
    !newNeeds <- guardRealloc (sizeOf a) needs
    let !(# cursor, newNeeds1 #) = getCursor newNeeds
    () <- toLinear (\cursor' -> L.fromSystemIO (poke (Ptr cursor') a)) cursor
    L.return (unsafeShiftNeedsPtr (sizeOf a) newNeeds1)

-- Internal

{-# INLINE guardRealloc #-}
guardRealloc :: Int -> Needs p1 t1 %1 -> L.IO (Needs p1 t1)
guardRealloc neededSpace needs =
    if neededSpace L.> spaceLeft
        then reallocNeeds needs1 neededSpace
        else L.return needs1
  where
    !(# spaceLeft, needs1 #) = getSpaceLeft needs

{-# INLINE reallocNeeds #-}
reallocNeeds :: Needs p t %1 -> Int -> L.IO (Needs p t)
reallocNeeds = toLinear reallocNeeds'

reallocNeeds' :: Needs p t -> Int -> L.IO (Needs p t)
reallocNeeds' (Needs origin cursor spaceLeft) additionalNeededSpace =
    let !cursorOffset = I# $ cursor `minusAddr#` mutableByteArrayContents# origin
        !oldBufferSize = spaceLeft + cursorOffset
        !newBufferSize = 2 * (oldBufferSize `max` additionalNeededSpace)
     in L.fromSystemIO
            ( IO $ \s -> case resizeMutableByteArray# origin (unInt newBufferSize) s of
                (# s', reallocedBA #) ->
                    (#
                        s'
                        , Needs
                            reallocedBA
                            (mutableByteArrayContents# reallocedBA `plusAddr#` unInt cursorOffset)
                            (newBufferSize - cursorOffset)
                    #)
            )

{-# NOINLINE appendNeeds #-}
appendNeeds :: Needs a b -> NeedsBuilder a' b' c d
appendNeeds (Needs srcMa cursor _) dest = L.do
    let !srcLen = cursor `minusAddr#` mutableByteArrayContents# srcMa
    !reallocedDest <- guardRealloc (I# srcLen) dest
    let %1 !(# destOg, reallocedDest1 #) = getOrigin reallocedDest
        %1 !() = toLinear (\destAddr -> case runRW# $ copyAddrToAddr# (mutableByteArrayContents# srcMa) destAddr srcLen of { !_ -> () }) destOg
    L.return (unsafeCastNeeds reallocedDest1)

-- Utils

{-# INLINE unsafeWriteTag #-}
unsafeWriteTag :: Int8 -> NeedsBuilder a b a' b
unsafeWriteTag tag needs = writeStorable tag (unsafeCastNeeds needs)

{-# INLINE unsafeShiftNeedsPtr #-}
unsafeShiftNeedsPtr :: Int -> Needs a b %1 -> Needs a' b
unsafeShiftNeedsPtr n (Needs og cursor spaceLeft) =
    Needs og (toLinear2 plusAddr# cursor (unInt n)) (spaceLeft L.- n)
