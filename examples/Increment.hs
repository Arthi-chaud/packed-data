{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Showcase how to 'mutate' packed data
module Increment (incrementRunner) where

import Data.Packed
import Data.Packed.Needs
import qualified Data.Packed.Needs as N
import qualified Data.Packed.Reader as R
import Tree

$(mkPacked ''Tree [])

incrementRunner :: Packed '[Tree Int] -> IO (Packed '[Tree Int])
incrementRunner p = runBuilder . fst <$> runReader incrementPacked p

-- -- The workhouse of the incrementation
incrementPacked :: PackedReader '[Tree Int] r (NeedsBuilder (Tree Int ': r1) '[Tree Int] r1 '[Tree Int])
incrementPacked =
    transformTree
        ( R.do
            n <- reader
            R.return (write (n + 1))
        )
        ( R.do
            left <- incrementPacked
            right <- incrementPacked
            R.return (\needs -> left needs N.>>= right)
        )
