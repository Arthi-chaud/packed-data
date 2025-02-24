-- | Showcase how to 'mutate' packed data
module Increment (incrementRunner) where

import Data.Packed
import Data.Packed.Needs
import qualified Data.Packed.Needs as N
import qualified Data.Packed.Reader as R
import Tree

$(mkPacked ''Tree [])

incrementRunner :: Packed '[Tree Int] -> IO (Packed '[Tree Int])
incrementRunner p = do
    (incremented, _) <- runReader incrementPacked p
    return $ finish incremented

-- The workhouse of the incrementation
incrementPacked :: PackedReader '[Tree Int] r (Needs '[] '[Tree Int])
incrementPacked =
    transformTree
        ( R.do
            n <- reader
            R.return (write (n + 1))
        )
        ( R.do
            left <- incrementPacked
            right <- incrementPacked
            R.return (applyNeeds left N.>> applyNeeds right)
        )
