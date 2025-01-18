module Data.Packed.TH.Skippable (genSkippableInstance) where

import Data.Packed.Skippable (Skippable (..))
import Data.Packed.TH.Flag
import Data.Packed.TH.Skip (genSkip, skipFName)
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- | Generates an instance of 'Skippable' for the given type
--
-- All the parameters of each constructor should be instances of 'Skippable'
--
-- __Example__
--
-- For the 'Tree' data type, it generates the following instance:
--
-- @
-- instance ('Skippable' a) => 'Skippable' (Tree a) where
--     skip = skipTree
-- @
genSkippableInstance :: [PackingFlag] -> Name -> Q [Dec]
genSkippableInstance flags tyName = do
    (resolvedType, typeParameterNames) <- resolveAppliedType tyName
    constraints <- mapM (\t -> [t|Skippable $(varT t)|]) typeParameterNames
    instanceType <- [t|Skippable $(return resolvedType)|]
    skipD <- genSkip flags tyName
    skipMethod <- funD 'skip [clause [] (normalB [|$(varE $ skipFName tyName)|]) []]
    return $
        skipD
            ++ [ InstanceD
                    (Just Overlapping)
                    constraints
                    instanceType
                    [skipMethod]
               ]
