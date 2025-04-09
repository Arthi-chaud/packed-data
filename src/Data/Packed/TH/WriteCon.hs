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
    Con ->
    -- | A unique (to the data type) 'Tag' to identify the packed data constructor.
    --
    -- For example, for a 'Tree' data type,
    -- we would typically use '0' for the 'Leaf' constructor and '1' for the 'Node' constructor
    Tag ->
    Q [Dec]
genConWrite flags con tag = do
    let (conName, _) = getNameAndBangTypesFromCon con
        r = VarT $ mkName "r"
        t = VarT $ mkName "t"
        fName = conWriteFName conName
        paramTypes = getConFieldsIdxAndNeedsFS con flags
    parentType <- do
        DataConI _ conTy _ <- reify conName
        return $ getParentTypeFromConstructorType conTy
    signature <- genConWriteSignature conName ((\(ty, _, _) -> ty) <$> paramTypes) parentType r t
    -- for each parameter type, we create a name
    fieldTypeAndName <- mapM (\ty -> (ty,) <$> newName "t") paramTypes
    body <-
        foldl
            ( \rest ((_, _, needsFS), paramName) ->
                -- We insert the size before
                if needsFS
                    then [|$rest N.>> writeWithFieldSize $(varE paramName)|]
                    else [|$rest N.>> write $(varE paramName)|]
            )
            [|$(varE $ startFName conName)|]
            fieldTypeAndName
    -- The pattern (lhs of '=' in a function implementation) will be something like '\a needs' for constructor 'Leaf a'
    let patt = VarP . snd <$> fieldTypeAndName
    start <- genStart flags con tag
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
