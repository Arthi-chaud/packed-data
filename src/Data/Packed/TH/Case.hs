{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Packed.TH.Case (caseFName, genCase) where

import Data.Packed.Reader hiding (return)
import Data.Packed.TH.Flag
import Data.Packed.TH.Utils (Tag, getBranchesTyList, getNameAndBangTypesFromCon, resolveAppliedType, sanitizeConName)
import GHC.Exts
import GHC.Word
import Language.Haskell.TH

caseFName :: Name -> Name
caseFName tyName = mkName $ "case" ++ sanitizeConName tyName

-- | Generates a function to allow pattern matching a packed data type using the data constructors
--
--  __Example:__
--
-- For the 'Tree' data type, it generates the following function:
--
-- @
-- caseTree ::
--     ('Data.Packed.PackedReader' '[a] r b) ->
--     ('Data.Packed.PackedReader' '[Tree a, Tree a] r b) ->
--     'Data.Packed.PackedReader' '[Tree a] r b
-- caseTree leafCase nodeCase = 'Data.Packed.Reader.mkPackedReader' $ \packed l -> do
--    (tag :: 'Tag', packed1, l1) <- 'Data.Packed.Unpackable.runReader' 'Data.Packed.reader' packed l
--    case tag of
--        0 -> 'Data.Packed.Reader.runReader' leafCase packed1 l1
--        1 -> 'Data.Packed.Reader.runReader' nodeCase packed1 l1
--        _ -> fail "Bad Tag"
-- @
genCase ::
    [PackingFlag] ->
    -- | The name of the type to generate the function for
    Name ->
    Q [Dec]
genCase flags tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    -- For each data constructor, we build names for the pattern for the case functions
    -- Example: leafCase, nodeCase, etc.
    let casePatterns = buildCaseFunctionName <$> cs
    body <- buildBody casePatterns
    signature <- genCaseSignature flags tyName
    return
        [ signature
        , FunD
            (caseFName tyName)
            [Clause (VarP <$> casePatterns) (NormalB body) []]
        ]
  where
    -- Build the body (the do, binding and case expressions)
    buildBody casePatterns =
        let bytes1VarName = mkName "b"
            length1VarName = mkName "l"
            flagVarName = mkName "flag"
         in do
                caseExpression <- buildCaseExpression flagVarName casePatterns bytes1VarName length1VarName
                [|
                    mkPackedReader $ \(Ptr addr) l' ->
                        let
                            !(# _, tag #) = runRW# (readWord8OffAddr# addr 0#)
                            !($(varP flagVarName)) = W8# tag
                            !($(varP bytes1VarName)) = Ptr (addr `plusAddr#` 1#)
                            !($(varP length1VarName)) = l' - 1
                         in
                            $(return caseExpression)
                    |]
    -- for dataconstructor Leaf, will be 'leafCase'
    buildCaseFunctionName = conNameToCaseFunctionName . fst . getNameAndBangTypesFromCon
    conNameToCaseFunctionName conName = mkName $ 'c' : (sanitizeConName conName) ++ "Case"

    -- Build the case .. of ... expression using the list of available xxxCase, the flag and bytestring
    buildCaseExpression :: Name -> [Name] -> Name -> Name -> Q Exp
    buildCaseExpression e casePatterns bytesVarName lengthVarName =
        -- For each xxxCase, we build a branch for the case expression
        let matches =
                ( \(conIndex, caseFuncName) -> do
                    body <- [|runPackedReader $(varE caseFuncName) $(varE bytesVarName) $(varE lengthVarName)|]
                    return $ Match (LitP $ IntegerL conIndex) (NormalB body) []
                )
                    <$> zip [0 ..] casePatterns
            fallbackMatch = do
                fallbackBody <- [|error ("Bad Tag: " ++ show $(varE $ mkName "err"))|]
                return $ Match (VarP $ mkName "err") (NormalB fallbackBody) []
         in caseE [|$(varE e) :: Tag|] $ matches ++ [fallbackMatch]

-- For a type 'Tree', generates the following signature
-- caseTree ::
--     ('Data.Packed.PackedReader' '[a] r b) ->
--     ('Data.Packed.PackedReader' '[Tree a, Tree a] r b) ->
--     'Data.Packed.PackedReader' '[Tree a] r b
genCaseSignature :: [PackingFlag] -> Name -> Q Dec
genCaseSignature flags tyName = do
    (sourceType, _) <- resolveAppliedType tyName
    bVar <- newName "b"
    rVar <- newName "r"
    branchesTypes <- getBranchesTyList tyName flags
    let
        bType = varT bVar
        rType = varT rVar
        lambdaTypes = (\branchTypes -> buildLambdaType branchTypes bType rType) <$> branchesTypes
        outType = [t|PackedReader '[$(return sourceType)] $rType $bType|]
    signature <- foldr (\lambda out -> [t|$lambda -> $out|]) outType lambdaTypes
    return $ SigD (caseFName tyName) signature
  where
    -- From a constructor (say Leaf a), build type PackedReader '[a] r b
    buildLambdaType :: [Type] -> Q Type -> Q Type -> Q Type
    buildLambdaType branchType returnType restType = do
        let branchTypeList = foldr (\a rest -> [t|$(return a) ': $rest|]) [t|'[]|] branchType
        [t|PackedReader $branchTypeList $restType $returnType|]
