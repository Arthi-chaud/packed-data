{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Packed.TH.Case (caseFName, genCase) where

import Data.Packed.Reader hiding (return)
import Data.Packed.TH.Flag
import Data.Packed.TH.Pattern (patternFName)
import Data.Packed.TH.Utils (getBranchesTyList, getNameAndBangTypesFromCon, resolveAppliedType, sanitizeConName)
import Language.Haskell.TH

caseFName :: Name -> Name
caseFName tyName = mkName $ "case" ++ sanitizeConName tyName

-- TODO Use generated patterns

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
-- caseTree leafCase nodeCase = 'Data.Packed.Reader.mkPackedReader' $ \case
--      PackedLeaf l -> 'runReaderStep' leafCase l
--      PackedNode l -> 'runReaderStep' nodeCase l
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
    let casePatterns = buildCaseCallbackNameAndPattern <$> cs
    body <- [|PackedReader $ $(buildCaseExpression casePatterns)|]
    signature <- genCaseSignature flags tyName
    return
        [ signature
        , FunD
            (caseFName tyName)
            [Clause (VarP . snd <$> casePatterns) (NormalB body) []]
        ]
  where
    -- for dataconstructor Leaf, will be 'leafCase'
    buildCaseCallbackNameAndPattern = (\name -> (patternFName name, conNameToCaseFunctionName name)) . fst . getNameAndBangTypesFromCon
    conNameToCaseFunctionName conName = mkName $ 'c' : (sanitizeConName conName) ++ "Case"

    -- Build the case .. of ... expression using the list of available xxxCase, the flag and bytestring
    buildCaseExpression :: [(Name, Name)] -> Q Exp
    buildCaseExpression casePatterns =
        -- For each xxxCase, we build a branch for the case expression
        let matches =
                ( \(patternName, caseCallbackName) -> do
                    let r = mkName "r"
                    body <- [|runReaderStep $(varE caseCallbackName) $(varE r)|]
                    let pat = ConP patternName [] [VarP r]
                    return $ Match pat (NormalB body) []
                )
                    <$> casePatterns
         in lamCaseE matches

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
