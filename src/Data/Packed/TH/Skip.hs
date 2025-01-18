module Data.Packed.TH.Skip (genSkip, skipFName) where

import Data.Packed.FieldSize (skipWithFieldSize)
import Data.Packed.Reader (PackedReader)
import qualified Data.Packed.Reader as R
import Data.Packed.Skippable (Skippable (skip))
import Data.Packed.TH.Case (caseFName)
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- For a data type 'Tree', will generate the function name 'skipTree'
skipFName :: Name -> Name
skipFName tyName = mkName $ "skip" ++ nameBase tyName

-- | Generates an function to skip a value of the given type in a 'Data.Packed.Packed'
--
--  __Example:__
--
-- For the 'Tree' data type, it generates the following function:
--
-- @
-- skipTree :: ('Data.Packed.Skippable' a) => 'Data.Packed.PackedReader' '[Tree a] r ()
-- skipTree = caseTree
--      'Data.Packed.Skip.skip'
--      ('skipTree' >> 'skipTree')
-- @
genSkip :: [PackingFlag] -> Name -> Q [Dec]
genSkip flags tyName = do
    let fName = skipFName tyName
    lambdas <- genSkipLambdas flags tyName
    funExpr <-
        foldl
            (\rest arg -> [|$rest $(return arg)|])
            (varE $ caseFName tyName)
            lambdas
    let fun = FunD fName [Clause [] (NormalB funExpr) []]
    signature <- genSkipSignature tyName
    return [signature, fun]

-- Generates all the lambda functions we will need, to skip using caseTree
genSkipLambdas :: [PackingFlag] -> Name -> Q [Exp]
genSkipLambdas flags tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    mapM
        ( \con ->
            let (_, bt) = getNameAndBangTypesFromCon con
             in genSkipLambda flags (snd <$> bt)
        )
        cs

-- generates a single lambda to use with caseTree for our skip method
genSkipLambda :: [PackingFlag] -> [Type] -> Q Exp
genSkipLambda flags conParameterTypes =
    foldr
        ( \hasSize ret ->
            let
                skipFSAndSkipField = [|skipWithFieldSize R.>> $ret|]
                skipField = [|skip R.>> $ret|]
             in
                if hasSize then skipFSAndSkipField else skipField
        )
        [|R.return ()|]
        $ (\i -> hasSizeFlag && (not skipLastFieldSizeFlag || (skipLastFieldSizeFlag && i /= length conParameterTypes - 1)))
            <$> [0 .. (length conParameterTypes - 1)]
  where
    hasSizeFlag = InsertFieldSize `elem` flags
    skipLastFieldSizeFlag = SkipLastFieldSize `elem` flags

-- Generates the following function signature for a data type 'Tree'
-- skipTree :: ('Data.Packed.Skippable' a) => 'Data.Packed.PackedReader' '[Tree a] r ()
genSkipSignature :: Name -> Q Dec
genSkipSignature tyName = do
    (sourceType, typeParameterNames) <- resolveAppliedType tyName
    let fName = skipFName tyName
        -- Type variables for Needs
        r = varT $ mkName "r"
        -- Define Skippable constraints on each of the type parameters
        constraints = mapM (\tyVarName -> [t|Skippable $(varT tyVarName)|]) typeParameterNames
        signature = [t|PackedReader '[$(return sourceType)] $r ()|]
    sigD fName (forallT [] constraints signature)
