module Data.Packed.TH.Transform (transformFName, genTransform) where

import Control.Monad
import Data.Maybe (catMaybes)
import Data.Packed.FieldSize (FieldSize)
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
--
--     ('Data.Packed.Reader.PackedReader' '[a] r ('Data.Packed.Needs.NeedsBuilder' (a ': r1) '[Tree a] r1 '[Tree a])) ->
--
--     ('Data.Packed.Reader.PackedReader' '[Tree a, Tree a] r ('Data.Packed.Needs.NeedsBuilder' (Tree a ': Tree a ': r1) '[Tree a] r1 '[Tree a])) ->
--     'Data.Packed.PackedReader' '[Tree a] r ('Data.Packed.NeedsBuilder' (Tree a ': r1) '[Tree a] r1 '[Tree a])
-- transformTree leafCase nodeCase = caseTree
--      (leafCase R.>>= \l -> return (startLeaf 'Data.Packed.Needs.>=>' l))
--      (nodeCase R.>>= \n -> return (startNode 'Data.Packed.Needs.>=>' n)))
genTransform :: [PackingFlag] -> Name -> Q [Dec]
genTransform flags tyName = do
    signature <- genTransformSignature flags tyName
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    body <-
        foldl
            ( \rest curr ->
                let caseName = buildCaseFunctionName curr
                 in if not $ conHasArguments curr
                        then [|$rest (R.return $(varE (startFNameForCon curr)))|]
                        else
                            [|
                                $rest
                                    ( $(varE caseName)
                                        R.>>= \resWriter ->
                                            R.return
                                                ( ($(varE (startFNameForCon curr)) N.>=> resWriter)
                                                )
                                    )
                                |]
            )
            (varE $ caseFName tyName)
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
    conNameToCaseFunctionName conName = mkName $ "caseCon" ++ (sanitizeConName conName)

    startFNameForCon = startFName . fst . getNameAndBangTypesFromCon
    conHasArguments = not . null . snd . getNameAndBangTypesFromCon

-- For a type 'Tree', generates the following signature
-- transformTree ::
--     ('Data.Packed.Reader.PackedReader' '[a] r ('Data.Packed.Needs.NeedsBuilder' (a ': r1) '[Tree a] r1 '[Tree a])) ->
--
--     ('Data.Packed.Reader.PackedReader' '[Tree a, Tree a] r ('Data.Packed.Needs.NeedsBuilder' (Tree a ': Tree a ': r1) '[Tree a] r1 '[Tree a])) ->
--     'Data.Packed.PackedReader' '[Tree a] r ('Data.Packed.NeedsBuilder' (Tree a ': r1) '[Tree a] r1 '[Tree a])
genTransformSignature :: [PackingFlag] -> Name -> Q Dec
genTransformSignature flags tyName = do
    (sourceType, _) <- resolveAppliedType tyName
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    rVar <- newName "r"
    r1Var <- newName "r1"
    let
        rType = varT rVar
        r1Type = varT r1Var
        lambdaTypes = (\c -> buildLambdaType c sourceType rType r1Type) <$> cs
        outType =
            [t|
                PackedReader
                    '[$(return sourceType)]
                    $rType
                    (N.NeedsBuilder ($(return sourceType) ': $(r1Type)) '[$(return sourceType)] $(r1Type) '[$(return sourceType)])
                |]
    signature <- foldr (\lambda out -> [t|$lambda -> $out|]) outType (catMaybes lambdaTypes)
    return $ SigD (transformFName tyName) signature
  where
    -- From a constructor (say Leaf a), build type PackedTransformer a r
    buildLambdaType con ty restType rest1Type =
        if null fieldType
            then Nothing
            else return $ do
                [readerType, builderType] <- forM [[t|'[]|], rest1Type] $ \r ->
                    foldr
                        ( \(fieldTy, _, needsFS) tys ->
                            if needsFS
                                then [t|FieldSize ': $(return fieldTy) ': $tys|]
                                else [t|$(return fieldTy) ': $tys|]
                        )
                        r
                        fieldType
                [t|
                    PackedReader
                        $(return readerType)
                        $restType
                        (N.NeedsBuilder $(return builderType) '[$(return ty)] $(rest1Type) '[$(return ty)])
                    |]
      where
        fieldType = getConFieldsIdxAndNeedsFS con flags
