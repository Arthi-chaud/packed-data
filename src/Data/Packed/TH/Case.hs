{-# LANGUAGE ScopedTypeVariables #-}

module Data.Packed.TH.Case (genCaseInstance) where

import Data.Packed.Case
import Data.Packed.Reader hiding (return)
import Data.Packed.TH.Flag
import Data.Packed.TH.Utils (Tag, getBranchesTyList, getNameAndBangTypesFromCon, resolveAppliedType, sanitizeConName)
import Language.Haskell.TH

genCaseInstance :: [PackingFlag] -> Name -> Q [Dec]
genCaseInstance flags tyName = do
    b <- newName "b"
    r <- newName "r"
    caseSig <- genCaseSig flags tyName b r
    caseBody <- genCaseFunc tyName
    (resolvedType, _) <- resolveAppliedType tyName
    instanceType <- [t|PackedCase $(return resolvedType) $(varT r) $(varT b)|]
    typeSyn <- [t|CaseSig $(return resolvedType) $(varT r) $(varT b)|]
    return
        [ InstanceD
            Nothing
            []
            instanceType
            ( TySynInstD (TySynEqn Nothing typeSyn caseSig)
                : caseBody -- TODO Incorrect
            )
        ]

-- | Generates the 'case_' function for the given type
--
--  __Example:__
--
-- For the 'Tree' data type, it generates the following function:
--
-- @
-- case_ ::
--     ('Data.Packed.PackedReader' '[a] r b) ->
--     ('Data.Packed.PackedReader' '[Tree a, Tree a] r b) ->
--     'Data.Packed.PackedReader' '[Tree a] r b
-- case_ leafCase nodeCase = 'Data.Packed.Reader.mkPackedReader' $ \packed l -> do
--    (tag :: 'Tag', packed1, l1) <- 'Data.Packed.Unpackable.runReader' 'Data.Packed.reader' packed l
--    case tag of
--        0 -> 'Data.Packed.Reader.runReader' leafCase packed1 l1
--        1 -> 'Data.Packed.Reader.runReader' nodeCase packed1 l1
--        _ -> fail "Bad Tag"
-- @
genCaseFunc ::
    -- | The name of the type to generate the function for
    Name ->
    Q [Dec]
genCaseFunc tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    packedName <- newName "packed"
    -- For each data constructor, we build names for the pattern for the case functions
    -- Example: leafCase, nodeCase, etc.
    let casePatterns = buildCaseFunctionName <$> cs
    body <- buildBody casePatterns packedName
    return
        [ FunD
            'case_
            [Clause (VarP <$> casePatterns) (NormalB body) []]
        ]
  where
    -- Build the body (the do, binding and case expressions)
    buildBody casePatterns packedName =
        let bytes1VarName = mkName "b"
            length1VarName = mkName "l"
            flagVarName = mkName "flag"
         in do
                caseExpression <- buildCaseExpression flagVarName casePatterns bytes1VarName length1VarName
                [|
                    mkPackedReader $ \($(varP packedName)) l' -> do
                        ($(varP flagVarName), $(varP bytes1VarName), $(varP length1VarName)) <- runPackedReader reader $(varE packedName) l'
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
                fallbackBody <- [|Prelude.fail "Bad Tag"|]
                return $ Match WildP (NormalB fallbackBody) []
         in caseE [|$(varE e) :: Tag|] $ matches ++ [fallbackMatch]

-- For a type 'Tree', generates the following type:
--
--  ('Data.Packed.PackedReader' '[a] r b) ->
--  ('Data.Packed.PackedReader' '[Tree a, Tree a] r b) ->
--  'Data.Packed.PackedReader' '[Tree a] r b
genCaseSig :: [PackingFlag] -> Name -> Name -> Name -> Q Type
genCaseSig flags tyName bName rName = do
    (sourceType, _) <- resolveAppliedType tyName
    branchesTypes <- getBranchesTyList tyName flags
    let
        bType = varT bName
        rType = varT rName
        lambdaTypes = (\branchTypes -> buildLambdaType branchTypes bType rType) <$> branchesTypes
        outType = [t|PackedReader '[$(return sourceType)] $rType $bType|]
    foldr (\lambda out -> [t|$lambda -> $out|]) outType lambdaTypes
  where
    -- From a constructor (say Leaf a), build type PackedReader '[a] r b
    buildLambdaType :: [Type] -> Q Type -> Q Type -> Q Type
    buildLambdaType branchType returnType restType = do
        let branchTypeList = foldr (\a rest -> [t|$(return a) ': $rest|]) [t|'[]|] branchType
        [t|PackedReader $branchTypeList $restType $returnType|]

-- -- | Generates a function to allow pattern matching a packed data type using the data constructors
-- --
-- --  __Example:__
-- --
-- -- For the 'Tree' data type, it generates the following function:
-- --
-- -- @
-- -- caseTree ::
-- --     ('Data.Packed.PackedReader' '[a] r b) ->
-- --     ('Data.Packed.PackedReader' '[Tree a, Tree a] r b) ->
-- --     'Data.Packed.PackedReader' '[Tree a] r b
-- -- caseTree leafCase nodeCase = 'Data.Packed.Reader.mkPackedReader' $ \packed l -> do
-- --    (tag :: 'Tag', packed1, l1) <- 'Data.Packed.Unpackable.runReader' 'Data.Packed.reader' packed l
-- --    case tag of
-- --        0 -> 'Data.Packed.Reader.runReader' leafCase packed1 l1
-- --        1 -> 'Data.Packed.Reader.runReader' nodeCase packed1 l1
-- --        _ -> fail "Bad Tag"
-- -- @
-- genCase ::
--     [PackingFlag] ->
--     -- | The name of the type to generate the function for
--     Name ->
--     Q [Dec]
-- genCase flags tyName = do
--     (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
--     packedName <- newName "packed"
--     -- For each data constructor, we build names for the pattern for the case functions
--     -- Example: leafCase, nodeCase, etc.
--     let casePatterns = buildCaseFunctionName <$> cs
--     body <- buildBody casePatterns packedName
--     signature <- genCaseSignature flags tyName
--     return
--         [ signature
--         , FunD
--             (caseFName tyName)
--             [Clause (VarP <$> casePatterns) (NormalB body) []]
--         ]
--   where
--     -- Build the body (the do, binding and case expressions)
--     buildBody casePatterns packedName =
--         let bytes1VarName = mkName "b"
--             length1VarName = mkName "l"
--             flagVarName = mkName "flag"
--          in do
--                 caseExpression <- buildCaseExpression flagVarName casePatterns bytes1VarName length1VarName
--                 [|
--                     mkPackedReader $ \($(varP packedName)) l' -> do
--                         ($(varP flagVarName), $(varP bytes1VarName), $(varP length1VarName)) <- runPackedReader reader $(varE packedName) l'
--                         $(return caseExpression)
--                     |]
--     -- for dataconstructor Leaf, will be 'leafCase'
--     buildCaseFunctionName = conNameToCaseFunctionName . fst . getNameAndBangTypesFromCon
--     conNameToCaseFunctionName conName = mkName $ 'c' : (sanitizeConName conName) ++ "Case"
--
--     -- Build the case .. of ... expression using the list of available xxxCase, the flag and bytestring
--     buildCaseExpression :: Name -> [Name] -> Name -> Name -> Q Exp
--     buildCaseExpression e casePatterns bytesVarName lengthVarName =
--         -- For each xxxCase, we build a branch for the case expression
--         let matches =
--                 ( \(conIndex, caseFuncName) -> do
--                     body <- [|runPackedReader $(varE caseFuncName) $(varE bytesVarName) $(varE lengthVarName)|]
--                     return $ Match (LitP $ IntegerL conIndex) (NormalB body) []
--                 )
--                     <$> zip [0 ..] casePatterns
--             fallbackMatch = do
--                 fallbackBody <- [|Prelude.fail "Bad Tag"|]
--                 return $ Match WildP (NormalB fallbackBody) []
--          in caseE [|$(varE e) :: Tag|] $ matches ++ [fallbackMatch]
--
-- -- For a type 'Tree', generates the following signature
-- -- caseTree ::
-- --     ('Data.Packed.PackedReader' '[a] r b) ->
-- --     ('Data.Packed.PackedReader' '[Tree a, Tree a] r b) ->
-- --     'Data.Packed.PackedReader' '[Tree a] r b
-- genCaseSignature :: [PackingFlag] -> Name -> Q Dec
-- genCaseSignature flags tyName = do
--     (sourceType, _) <- resolveAppliedType tyName
--     bVar <- newName "b"
--     rVar <- newName "r"
--     branchesTypes <- getBranchesTyList tyName flags
--     let
--         bType = varT bVar
--         rType = varT rVar
--         lambdaTypes = (\branchTypes -> buildLambdaType branchTypes bType rType) <$> branchesTypes
--         outType = [t|PackedReader '[$(return sourceType)] $rType $bType|]
--     signature <- foldr (\lambda out -> [t|$lambda -> $out|]) outType lambdaTypes
--     return $ SigD (caseFName tyName) signature
--   where
--     -- From a constructor (say Leaf a), build type PackedReader '[a] r b
--     buildLambdaType :: [Type] -> Q Type -> Q Type -> Q Type
--     buildLambdaType branchType returnType restType = do
--         let branchTypeList = foldr (\a rest -> [t|$(return a) ': $rest|]) [t|'[]|] branchType
--         [t|PackedReader $branchTypeList $restType $returnType|]
