module Data.Packed.TH.RepackCon (genConstructorRepackers) where

import Data.Packed.FieldSize
import Data.Packed.Needs (Needs, applyNeeds)
import qualified Data.Packed.Needs as N
import Data.Packed.Packed (Packed)
import Data.Packed.TH.Flag
import Data.Packed.TH.Start (startFName)
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- | Generates a function that builds back data using already serialised fields
--
-- __Example:__
--
-- For the 'Tree' data type, it generates the following functions
--
-- @
-- repackLeaf :: 'Data.Packed.Needs' '[] a -> IO ('Data.Packed.Packed' '[Tree a])
-- repackLeaf pval = withEmptyNeeds (startLeaf N.>> 'Data.Packed.Needs.concatNeeds' pval)
--
-- repackNode :: 'Data.Packed.Needs' '[] (Tree a) -> 'Data.Packed.Needs' '[] (Tree a) -> IO ('Data.Packed.Packed  '[Tree a])
-- repackNode lval rval needs = N.runBuilder (startNode needs N.>>= 'concatNeeds' lval N.>>= 'concatNeeds' rval)
-- @
genConstructorRepackers :: [PackingFlag] -> Name -> Q [Dec]
genConstructorRepackers flags tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    packers <- genConstructorRepacker flags `mapM` cs
    return $ concat packers

repackConFName :: Name -> Name
repackConFName conName = mkName $ "repack" ++ sanitizeConName conName

genConstructorRepacker :: [PackingFlag] -> Con -> Q [Dec]
genConstructorRepacker flags con = do
    let conName = fst $ getNameAndBangTypesFromCon con
        fieldTypes = getConFieldsIdxAndNeedsFS con flags
        needsName = mkName "needs"
    varNames <- mapM (\_ -> newName "t") fieldTypes
    writeExp <-
        let concated =
                foldl
                    ( \rest ((_, _, needsFieldSize), varName) ->
                        if needsFieldSize
                            then [|($rest) N.>>= applyNeedsWithFieldSize $(varE varName)|]
                            else [|($rest) N.>>= applyNeeds $(varE varName)|]
                    )
                    [|$(varE $ startFName conName) $(varE needsName)|]
                    (zip fieldTypes varNames)
         in [|N.runBuilder $ \($(varP needsName)) -> $concated|]
    signature <- genConstructorPackerSig flags conName ((\(t, _, _) -> t) <$> fieldTypes)
    return
        [ signature
        , FunD (repackConFName conName) [Clause (VarP <$> varNames) (NormalB writeExp) []]
        ]

genConstructorPackerSig :: [PackingFlag] -> Name -> [Type] -> Q Dec
genConstructorPackerSig _ conName argTypes = do
    (DataConI _ _ tyName) <- reify conName
    (ty, _) <- resolveAppliedType tyName
    signature <- foldr (\p rest -> [t|Needs '[] '[$(return p)] -> $rest|]) [t|IO (Packed '[$(return ty)])|] argTypes
    return $ SigD (repackConFName conName) $ ForallT [] [] signature
