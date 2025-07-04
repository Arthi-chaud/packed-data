{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Packed.TH.Pattern (genPatterns, patternFName, tagReaderFName) where

import Control.Monad (forM)
import Data.Packed.Reader (PackedFragment (..))
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Utils
import GHC.Exts
import GHC.Ptr
import Language.Haskell.TH

-- | Generates patterns for packed values of the given type
--
--  __Example:__
--
-- For the 'Tree' data type, it generates the following patterns:
--
-- @
-- PackedLeaf :: (PackedFragment (a ': r)) -> PackedFragment (Tree a ': r)
--
-- PackedNode :: (PackedFragment (Tree A ': Tree a ': r)) -> PackedFragment (Tree a ': r)
-- @
--
-- More specifically, it generates the following code:
--
-- @
--
-- {-# INLINE readTreeTag #-}
-- readTreeTag :: PackedFragment (Tree a ': r) -> (# Word8#, PackedFragment n #)
-- readTreeTag (PF ptr@(Ptr addr) i) = case readWord8OffAddr# addr 0# realWorld# of
--     (# _, t #) -> case t of
--         0 -> (# t, PF (ptr `plusPtr` 1) (i - 1) #)
--         1 -> (# t, PF (ptr `plusPtr` 1) (i - 1) #)
--         _ -> error $ "Bag tag: Got " ++ show (W8# t)
--
-- {-# INLINE PackedLeaf #-}
-- pattern PackedLeaf :: PackedFragment (a ': r) -> 'PackedFragment' (Tree a ': r)
-- pattern PackedLeaf pf <- (readTreeTag -> (# 0, pf #))
--
-- {-# INLINE PackedNode #-}
-- pattern PackedNode :: 'PackedFragment' (Tree a ': Tree a ': r) -> 'PackedFragment' (Tree a ': r)
-- pattern PackedNode pf <- (readTreeTag -> (# 1, pf #))
--
-- {-# COMPLETE PackedLeaf, PackedNode #-}
-- @
genPatterns :: [PackingFlag] -> Name -> Q [Dec]
genPatterns flags tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    patterns <- concat <$> forM (zip cs [0 ..]) (uncurry (genPattern flags tyName))
    tagReader <- genTagReader flags tyName
    let completePragma =
            let
                patternNames = patternFName . fst . getNameAndBangTypesFromCon <$> cs
             in
                CompleteP patternNames Nothing
    return $ tagReader ++ patterns ++ [PragmaD completePragma]

genPattern :: [PackingFlag] -> Name -> Con -> Integer -> Q [Dec]
genPattern flags tyName con conIdx = do
    let patternName = patternFName $ fst $ getNameAndBangTypesFromCon con
    pfName <- newName "pf"
    (ty, _) <- resolveAppliedType tyName
    rVar <- newName "r"
    conArgTypes <- getBranchTyList con flags
    patternType <- do
        patternFieldType <- foldr (\curr rest -> [t|$(return curr) ': $rest|]) (varT rVar) conArgTypes
        fromType <- [t|$(return ty) ': $(varT rVar)|]
        [t|PackedFragment ($(return patternFieldType)) -> PackedFragment ($(return fromType))|]
    let patSynSig =
            PatSynSigD
                patternName
                patternType
    patSyn <- do
        let viewPat = [p|(# $(litP $ WordPrimL conIdx), $(varP pfName) #)|]
        patSynD
            patternName
            (prefixPatSyn [pfName])
            unidir
            (viewP (varE $ tagReaderFName tyName) viewPat)

    return
        [ PragmaD (InlineP patternName Inline FunLike AllPhases)
        , patSynSig
        , patSyn
        ]

patternFName :: Name -> Name
patternFName name = mkName $ "Packed" ++ sanitizeConName name

genTagReader :: [PackingFlag] -> Name -> Q [Dec]
genTagReader flags tyName = do
    rType <- newName "r"
    (ty, _) <- resolveAppliedType tyName
    conTypes <- getBranchesTyList tyName flags
    let fName = tagReaderFName tyName
    prototype <-
        let
            inType = [t|PackedFragment ($(return ty) ': $(varT rType))|]
            outType = [t|(# Word#, PackedFragment $(varT $ mkName "n") #)|]
         in
            [t|$inType -> $outType|]
    (pat, body) <- do
        let addrName = mkName "addr"
            ptrName = mkName "ptr"
            lengthName = mkName "l"
            tagVarName = mkName "t"
        readWordExp <- [|readWord8OffAddr# $(varE addrName) 0# realWorld#|]
        caseBranches <- forM (zip [0 ..] conTypes) $ \(conIdx, _) -> do
            tName <- newName "t"
            let pat = AsP tName (LitP $ WordPrimL conIdx)
            branchExp <- [|(# $(varE tName), PF ($(varE ptrName) `plusPtr` 1) ($(varE lengthName) - 1) #)|]
            return $ Match pat (NormalB branchExp) []
        errorBranch <- do
            tName <- newName "t"
            match (asP tName wildP) (normalB [|error $ "Bad tag, got " ++ show (W# $(varE tName))|]) []
        let caseExpr = caseE [|word8ToWord# $(varE tagVarName)|] (return <$> (caseBranches ++ [errorBranch]))
        body <- [|case $(return readWordExp) of (# _, $(varP tagVarName) #) -> $caseExpr|]
        pattern_ <- do
            ptrPattern <- asP ptrName [p|Ptr $(varP addrName)|]
            [p|(PF $(return ptrPattern) $(varP lengthName))|]
        return (pattern_, body)

    return
        [ PragmaD (InlineP fName Inline FunLike AllPhases)
        , SigD fName prototype
        , FunD fName [Clause [pat] (NormalB body) []]
        ]

tagReaderFName :: Name -> Name
tagReaderFName tyName = mkName $ "read" ++ sanitizeConName tyName ++ "Tag"
