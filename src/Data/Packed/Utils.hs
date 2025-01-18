{-# LANGUAGE TypeFamilies #-}

module Data.Packed.Utils ((:++:)) where

-- | Type operator to concat lists of types
type family a :++: b where
    '[] :++: xs = xs
    (x ': xs) :++: r = x ': (xs :++: r)

-- class
--     (:++:) (xs :: [Type]) (ys :: [Type]) (zs :: [Type])
--         | zs ys -> xs
--         , zs xs -> ys
--
-- instance (ys ~ zs) => (:++:) '[] ys zs
--
-- instance ((:++:) xs ys zs) => (:++:) (x : xs) ys (x : zs)
