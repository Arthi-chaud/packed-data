module Data.Packed.TH.Utils (
    Tag,
    getParentTypeFromConstructorType,
    resolveAppliedType,
    getNameAndBangTypesFromCon,
    sanitizeConName,
) where

import Data.Char
import Data.Word (Word8)
import Language.Haskell.TH

-- | Byte in a 'Data.Packed' value to identify which data constructor is serialised
type Tag = Word8

getParentTypeFromConstructorType :: Type -> Type
getParentTypeFromConstructorType (ForallT _ _ t) = getParentTypeFromConstructorType t
getParentTypeFromConstructorType t@(AppT _ (VarT _)) = t
getParentTypeFromConstructorType (AppT _ t) = getParentTypeFromConstructorType t
getParentTypeFromConstructorType x = x

-- From a type, returns the fully applied type with type variables' names
-- For a type 'Tree', will return (Tree a, [a])
resolveAppliedType :: Name -> Q (Type, [Name])
resolveAppliedType tyName = do
    (TyConI (DataD _ _ boundTypeVar _ _ _)) <- reify tyName
    -- Extract already existing type names from types variables bound to source type
    let typeParameterNames =
            ( \case
                (KindedTV n _ _) -> n
                x -> error $ "unhandled type parameter" ++ show x
            )
                <$> boundTypeVar
    -- Builds back 'Tree a' using type variable names (fold by applying each of them to the source type name)
    sourceType <- foldl (\ty par -> [t|$ty $(varT par)|]) (conT tyName) typeParameterNames
    return (sourceType, typeParameterNames)

getNameAndBangTypesFromCon :: Con -> (Name, [BangType])
getNameAndBangTypesFromCon (NormalC name bt) = (name, bt)
getNameAndBangTypesFromCon (RecC name nbt) = (name, (\(_, b, t) -> (b, t)) <$> nbt)
getNameAndBangTypesFromCon (InfixC bt1 name bt2) = (name, [bt1, bt2])
getNameAndBangTypesFromCon (ForallC _ _ con) = getNameAndBangTypesFromCon con
getNameAndBangTypesFromCon (GadtC (name : _) bt _) = (name, bt)
getNameAndBangTypesFromCon (RecGadtC (name : _) nbt _) = (name, (\(_, b, t) -> (b, t)) <$> nbt)
getNameAndBangTypesFromCon x = error $ "unhandled data constructor: " ++ show x

-- | Sanitize constructor name so that it can be used as a symbol name
sanitizeConName :: Name -> String
sanitizeConName conName = strName $ nameBase conName
  where
    strName s = (\c -> if isAlphaNum c then [c] else show $ ord c) =<< s
