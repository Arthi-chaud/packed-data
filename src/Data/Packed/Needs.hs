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
import Unsafe.Linear
import Prelude hiding ((>>=))
import GHC.IO (unsafeDupablePerformIO)

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

type NeedsBuilder p1 t1 p2 t2 = Needs p1 t1 %1 -> Identity (Needs p2 t2)

{-# INLINE (>>=) #-}
(>>=) ::  Identity (Needs p1 t1) %1 -> (Needs p1 t1 %1 -> Identity (Needs p2 t2)) -> Identity (Needs p2 t2)
(>>=) a b = L.do
    !x <- a
    !x1 <- b x
    L.return x1

{-# INLINE (>=>) #-}
(>=>) :: (L.Monad m) => (a %1 -> m t ) %1 -> (t %1 -> m b) -> a %1 -> m b
(>=>) a b !c = L.do
    !x <- a c
    !x1 <- b x
    L.return x1


--- | Shortcut type for 'NeedsBuilder'\'s that simply write a value to a 'Needs' without changing the final packed type
type NeedsWriter a r t = NeedsBuilder (a ': r) t r t

--- | Shortcut type for 'NeedsBuilder'\'s that simply write multiple values to a 'Needs' without changing the final packed type
type NeedsWriter' a r t = NeedsBuilder (a :++: r) t r t


{-# NOINLINE runBuilder #-}
runBuilder :: NeedsBuilder p1 r '[] r -> IO (Packed r)
runBuilder builder =
    let !(# _, byteArray #) = runRW# $ newPinnedByteArray# 100#
        !addr = mutableByteArrayContents# byteArray
        !srcNeeds = Needs byteArray addr 100
        !finalNeeds = runIdentity $! builder srcNeeds
    in finish finalNeeds

{-# NOINLINE finish #-}
finish :: Needs '[] a ->  IO (Packed a)
finish (Needs og cursor _) = 
    let !contentLen = cursor `minusAddr#` mutableByteArrayContents# og
        !(# _, resizedBA #) = runRW# $ resizeMutableByteArray# og contentLen
        -- SRC: https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.ForeignPtr.html#mallocPlainForeignPtrBytes
        !fptr = ForeignPtr (mutableByteArrayContents# resizedBA) (PlainPtr resizedBA)
    -- BS calls this function when alloacting an ForeignPtr, let's do that too
    -- https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/src/Data.ByteString.Internal.Type.html#mkDeferredByteString
    in unsafeToPacked <$> mkDeferredByteString fptr (I# contentLen)

{-# INLINE concatNeeds #-}
concatNeeds :: Needs p t %1 -> NeedsBuilder '[] t1 p (t1 :++: t)
concatNeeds = toLinear2 (\src dest -> return $ appendNeeds src dest)

{-# INLINE applyNeeds #-}
applyNeeds :: Needs '[] t1 %1 -> NeedsBuilder (t1 :++: r) t r t
applyNeeds = toLinear2 (\src dest -> return $ appendNeeds src dest)

{-# INLINE writeStorable #-}
writeStorable :: (Storable a) => a -> NeedsBuilder (a ': r) t r t
writeStorable a needs =
    let !sizeOfA = sizeOf a
        !newNeeds = guardRealloc sizeOfA needs
        !(# cursor, newNeeds1 #) = getCursor newNeeds
        %1 !() = toLinear (\cursor' -> unsafeDupablePerformIO (poke (Ptr cursor')  a)) cursor
     in L.return (unsafeShiftNeedsPtr sizeOfA newNeeds1)

-- Internal

{-# INLINE guardRealloc #-}
guardRealloc :: Int -> Needs p1 t1 %1 -> Needs p1 t1
guardRealloc neededSpace needs =
    if neededSpace L.> spaceLeft
        then reallocNeeds needs1 neededSpace
        else needs1
  where
    !(# spaceLeft, needs1 #) = getSpaceLeft needs

{-# NOINLINE reallocNeeds #-}
reallocNeeds :: Needs p t %1 -> Int -> Needs p t
reallocNeeds needs additionalNeededSpace =
    toLinear
        ( \(Needs origin cursor spaceLeft) ->
            let 
                !cursorOffset = I# $ cursor `minusAddr#` mutableByteArrayContents# origin
                !bufferSize = spaceLeft + cursorOffset
                !newBufferSize =  (2 * (bufferSize `max` additionalNeededSpace))
                !(# _, reallocedBA #) = runRW# (resizeMutableByteArray# origin ( unInt newBufferSize))
             in Needs
                    reallocedBA
                    (mutableByteArrayContents# reallocedBA `plusAddr#` unInt cursorOffset)
                    (newBufferSize - cursorOffset)
        )
        needs

{-# NOINLINE appendNeeds #-}
appendNeeds :: Needs a b -> Needs a' b' -> Needs c d
appendNeeds (Needs srcMa cursor _) dest =
    let
        !srcLen = cursor `minusAddr#` mutableByteArrayContents# srcMa
        !dest1@(Needs ma _ _) = guardRealloc (I# srcLen) dest
        !_ = runRW# $ copyAddrToAddr# (mutableByteArrayContents# srcMa) (mutableByteArrayContents# ma)  srcLen
     in
        unsafeCastNeeds dest1

-- Utils

{-# INLINE unsafeWriteTag #-}
unsafeWriteTag :: Int8 -> NeedsBuilder a b a' b
unsafeWriteTag tag needs = writeStorable tag (unsafeCastNeeds needs)

{-# INLINE unsafeShiftNeedsPtr #-}
unsafeShiftNeedsPtr :: Int -> Needs a b %1 -> Needs a' b
unsafeShiftNeedsPtr n (Needs og cursor spaceLeft) =
    Needs og (toLinear2 plusAddr# cursor (unInt n)) (spaceLeft L.- n)
