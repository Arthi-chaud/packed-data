{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Data.Packed.TH.WriteCon (genConWrite, conWriteFName) where

import Data.List (group, sort)
import Data.Packed.FieldSize
import Data.Packed.Needs (NeedsWriter)
import qualified Data.Packed.Needs as N
import Data.Packed.Packable
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Start (genStart, startFName)
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- For a constructor 'Leaf', will generate the function name 'writeConLeaf'
conWriteFName :: Name -> Name
conWriteFName conName = mkName $ "writeCon" ++ sanitizeConName conName

-- | Generates a function that serialises and write a value to a 'Needs'.
-- The generated function is specific to a single data constructor.
--
-- __Example:__
--
-- For the 'Tree' data type, it generates the following function for the 'Leaf' constructor
--
-- @
-- writeConLeaf :: ('Packable' a) => a -> 'NeedsWriter (Tree a) r t'
-- writeConLeaf n  = startLeaf 'Data.Packed.Needs.>>' 'write' n
-- @
genConWrite ::
    [PackingFlag] ->
    -- | The name of the data constructor to generate the function for
    Name ->
    -- | A unique (to the data type) 'Tag' to identify the packed data constructor.
    --
    -- For example, for a 'Tree' data type,
    -- we would typically use '0' for the 'Leaf' constructor and '1' for the 'Node' constructor
    Tag ->
    [BangType] ->
    Q [Dec]
genConWrite flags conName conIndex bangTypes = do
    (DataConI _ conType _) <- reify conName
    let r = VarT $ mkName "r"
        t = VarT $ mkName "t"
        fName = conWriteFName conName
        paramTypeList = snd <$> bangTypes
        parentType = getParentTypeFromConstructorType conType
    signature <- genConWriteSignature conName paramTypeList parentType r t
    -- for each parameter type, we create a name
    varNameAndType <- mapM (\ty -> (,ty) <$> newName "t") paramTypeList
    -- we either call `encode` for every type parameter, and fold
    body <-
        foldl
            ( \rest (paramName, needsSizeTag) ->
                -- We insert the size before
                if needsSizeTag
                    then [|$rest N.>> writeWithFieldSize $(varE paramName)|]
                    else [|$rest N.>> write $(varE paramName)|]
            )
            [|$(varE $ startFName conName)|]
            ( if InsertFieldSize `elem` flags
                then case reverse varNameAndType of
                    -- Here, 'a' is the last field. We insert a FieldSize iff SkipLastFieldSize is not set
                    (a : b) -> reverse $ (fst a, SkipLastFieldSize `notElem` flags) : ((,True) . fst <$> b)
                    x -> reverse $ (,True) . fst <$> x
                else (,False) . fst <$> varNameAndType
            )
    -- The pattern (lhs of '=' in a function implementation) will be something like '\a needs' for constructor 'Leaf a'
    let patt = VarP . fst <$> varNameAndType
    start <- genStart flags conName conIndex (snd <$> bangTypes)
    return $
        start
            ++ [ signature
               , FunD fName [Clause [] (NormalB $ LamE patt body) []]
               ]

-- Generates the function signature for functions like 'writeConLeaf'
-- writeConLeaf :: ('Packable' a) => a -> 'NeedsWriter (Tree a) r t'
genConWriteSignature :: Name -> [Type] -> Type -> Type -> Type -> Q Dec
genConWriteSignature constructorName constructorArgumentsTypes parentType restType resultType = do
    let funName = conWriteFName constructorName
        typeVariables = filterDuplicates $ concatMap getAllVarInType constructorArgumentsTypes
        -- The signature without the constructor's parameters
        needsWriterType = [t|NeedsWriter $(return parentType) $(return restType) $(return resultType)|]
        constraints = mapM (\tyVar -> [t|(Packable $(return tyVar))|]) typeVariables
        funSignature = foldr (\p rest -> [t|$(return p) -> $rest|]) needsWriterType constructorArgumentsTypes
    sigD funName $ forallT [] constraints funSignature
  where
    getAllVarInType (AppT a b) = getAllVarInType a ++ getAllVarInType b
    getAllVarInType v@(VarT _) = [v]
    getAllVarInType _ = []
    filterDuplicates = map head . sort . group
