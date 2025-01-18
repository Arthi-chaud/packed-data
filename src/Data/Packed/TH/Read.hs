{-# LANGUAGE QualifiedDo #-}

module Data.Packed.TH.Read (readFName, genRead) where

import Data.Packed.Reader hiding (return)
import qualified Data.Packed.Reader as R
import Data.Packed.TH.Case (caseFName)
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Utils
import Data.Packed.Unpackable
import Language.Haskell.TH

readFName :: Name -> Name
readFName tyName = mkName $ "read" ++ nameBase tyName

-- | Generates an function to read (i.e. deserialise) the given data type.
--
--  __Example:__
--
-- For the 'Tree' data type, it generates the following function:
--
-- @
-- readTree :: ('Unpackable' a) => 'Data.Packed.PackedReader' '[Tree a] r (Tree a)
-- readTree = caseTree
--     ('Data.Packed.reader' >>= \\leafContent ->
--          'Data.Packed.Reader.return' $ Leaf leafContent
--     )
--
--     ('Data.Packed.reader' >>= \\leftContent ->
--      'Data.Packed.reader' >>= \\rightContent ->
--          'Data.Packed.Reader.return' $ Node leftContent rightContent
--     )
-- @
--
-- __Note__ We use bindings ('Data.Packed.Reader.>>=') intead of a do-notation, since 'Data.Packed.Reader' is not a monad. It's an indexed monad, meaning that the user would have to enable the 'QualifiedDo' extenstion for it to compile.
genRead ::
    [PackingFlag] ->
    Name ->
    -- | The name of the type to generate the function for
    Q [Dec]
genRead flags tyName = do
    let fName = readFName tyName
    (resolvedType, typeVariables) <- resolveAppliedType tyName
    lambdas <- genReadLambdas flags tyName
    -- we fold the list of lambda by applring them to `caseTree packed`
    funExpr <-
        foldl
            (\rest arg -> [|$rest $(return arg)|])
            (varE $ caseFName tyName)
            lambdas
    let fun = FunD fName [Clause [] (NormalB funExpr) []]
    signature <- genReadSignature tyName resolvedType typeVariables
    return [signature, fun]

-- Generates all the lambda functions we will need, to unpack using caseTree
genReadLambdas :: [PackingFlag] -> Name -> Q [Exp]
genReadLambdas flags tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    mapM
        ( \con ->
            let (conName, bt) = getNameAndBangTypesFromCon con
             in genReadLambda flags conName (snd <$> bt)
        )
        cs

-- generates a single lambda to use with caseTree for our unpack method
genReadLambda :: [PackingFlag] -> Name -> [Type] -> Q Exp
genReadLambda flags conName conParameterTypes = do
    let appliedConstructor =
            foldl
                (\rest arg -> AppE rest $ VarE arg)
                (ConE conName)
                $ (\i -> mkName $ "arg" ++ show i)
                    <$> [0 .. (length conParameterTypes - 1)]
    buildBindingExpression appliedConstructor
  where
    hasSizeFlag = InsertFieldSize `elem` flags
    skipLastFieldSizeFlag = SkipLastFieldSize `elem` flags
    buildBindingExpression :: Exp -> Q Exp
    buildBindingExpression appliedConstructor =
        foldr
            ( \(argIndex, hasSize) ret ->
                let
                    skipAndUnpack = [|skip R.>> $unpackExpr|]
                    unpackExpr = [|reader R.>>= \($(varP $ mkName $ "arg" ++ show argIndex)) -> $ret|]
                 in
                    if hasSize then skipAndUnpack else unpackExpr
            )
            [|R.return ($(parensE (return appliedConstructor)))|]
            $ (\i -> (i, hasSizeFlag && (not skipLastFieldSizeFlag || (skipLastFieldSizeFlag && i /= length conParameterTypes - 1))))
                <$> [0 .. (length conParameterTypes - 1)]

-- For a type 'Tree', generates the following function signature
-- readTree :: ('Unpackable' a) => 'Data.Packed.PackedReader' '[Tree a] r (Tree a)
genReadSignature :: Name -> Type -> [Name] -> Q Dec
genReadSignature tyName resolvedType typeVariables = do
    restTypeName <- newName "r"
    let readerType = [t|PackedReader '[$(return resolvedType)] $(varT restTypeName) ($(return resolvedType))|]
        constraints = mapM (\tyVarName -> [t|Unpackable $(varT tyVarName)|]) typeVariables
        signature = readerType
    sigD (readFName tyName) $ forallT [] constraints signature
