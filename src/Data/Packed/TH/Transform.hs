module Data.Packed.TH.Transform (transformFName, genTransform) where

import Data.Maybe (catMaybes)
import Data.Packed.FieldSize (FieldSize)
import Data.Packed.Needs (withEmptyNeeds)
import qualified Data.Packed.Needs as N
import Data.Packed.Reader (PackedReader)
import qualified Data.Packed.Reader as R
import Data.Packed.TH.Case (caseFName)
import Data.Packed.TH.Flag
import Data.Packed.TH.Start (startFName)
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- | For a constructor 'Leaf', will generate the function name 'transformLeaf'
transformFName :: Name -> Name
transformFName conName = mkName $ "transform" ++ sanitizeConName conName

-- For a type 'Tree', generates the following function
--
-- transformTree ::
--     ('Data.Packed.Reader.PackedReader' '[a] r ('Data.Packed.Needs.NeedsBuilder' '[a] '[Tree a] '[] '[Tree a])) ->
--
--     ('Data.Packed.Reader.PackedReader' '[Tree a, Tree a] r ('Data.Packed.Needs.NeedsBuilder' '[Tree a, Tree a] '[Tree a] '[] '[Tree a])) ->
--     'Data.Packed.PackedReader' '[Tree a] r ('Data.Packed.Needs' '[] '[Tree a])
-- transformTree leafCase nodeCase = caseTree
--      (leafCase R.>>= \l -> 'Data.Packed.Needs.finish' ('Data.Packed.Needs.withEmptyNeeds' (startLeaf 'Data.Packed.Needs.>>' l)))
--      (nodeCase R.>>= \n -> 'Data.Packed.Needs.finish' ('Data.Packed.Needs.withEmptyNeeds' (startNode 'Data.Packed.Needs.>>' n)))
genTransform :: [PackingFlag] -> Name -> Q [Dec]
genTransform flags tyName = do
    signature <- genTransformSignature flags tyName
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    body <-
        foldl
            ( \rest curr ->
                let caseName = buildCaseFunctionName curr
                 in if not $ conHasArguments curr
                        then [|$rest (R.return (withEmptyNeeds $(varE (startFNameForCon curr))))|]
                        else [|$rest ($(varE caseName) R.>>= \resWriter -> R.return (withEmptyNeeds ($(varE (startFNameForCon curr)) N.>> resWriter)))|]
            )
            (varE $ caseFNameForType tyName)
            cs
    return
        [ signature
        , FunD
            (transformFName tyName)
            [Clause (VarP . buildCaseFunctionName <$> filter conHasArguments cs) (NormalB body) []]
        ]
  where
    -- for dataconstructor Leaf, will be 'leafCase'
    buildCaseFunctionName = conNameToCaseFunctionName . fst . getNameAndBangTypesFromCon
    conNameToCaseFunctionName conName = mkName $ "case_" ++ (sanitizeConName conName)

    startFNameForCon = startFName . fst . getNameAndBangTypesFromCon
    caseFNameForType = caseFName
    conHasArguments = not . null . snd . getNameAndBangTypesFromCon

-- For a type 'Tree', generates the following signature
-- transformTree ::
--     ('Data.Packed.Reader.PackedReader' '[a] r ('Data.Packed.Needs.NeedsBuilder' '[a] '[Tree a] '[] '[Tree a])) ->
--
--     ('Data.Packed.Reader.PackedReader' '[Tree a, Tree a] r ('Data.Packed.Needs.NeedsBuilder' '[Tree a, Tree a] '[Tree a] '[] '[Tree a])) ->
--     'Data.Packed.PackedReader' '[Tree a] r ('Data.Packed.Needs' '[] '[Tree a])
genTransformSignature :: [PackingFlag] -> Name -> Q Dec
genTransformSignature flags tyName = do
    (sourceType, _) <- resolveAppliedType tyName
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    rVar <- newName "r"
    let
        rType = varT rVar
        lambdaTypes = (\c -> buildLambdaType c sourceType rType) <$> cs
        outType =
            [t|
                PackedReader
                    '[$(return sourceType)]
                    $rType
                    (N.Needs '[] '[$(return sourceType)])
                |]
    signature <- foldr (\lambda out -> [t|$lambda -> $out|]) outType (catMaybes lambdaTypes)
    return $ SigD (transformFName tyName) signature
  where
    -- From a constructor (say Leaf a), build type PackedTransformer a r
    buildLambdaType con ty restType =
        if null fieldType
            then Nothing
            else return $ do
                packedContentType <-
                    foldr
                        ( \(fieldTy, _, needsFS) tys ->
                            if needsFS
                                then [t|FieldSize ': $(return fieldTy) ': $tys|]
                                else [t|$(return fieldTy) ': $tys|]
                        )
                        [t|'[]|]
                        fieldType
                [t|
                    PackedReader
                        $(return packedContentType)
                        $restType
                        (N.NeedsBuilder $(return packedContentType) '[$(return ty)] '[] '[$(return ty)])
                    |]
      where
        fieldType = getConFieldsIdxAndNeedsFS con flags
