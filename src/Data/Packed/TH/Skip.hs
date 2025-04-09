module Data.Packed.TH.Skip (genSkip, skipFName) where

import Data.Packed.Case (case_)
import Data.Packed.FieldSize (skipWithFieldSize)
import Data.Packed.Reader (PackedReader)
import qualified Data.Packed.Reader as R
import Data.Packed.Skippable (Skippable (skip))
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Utils
import Language.Haskell.TH

-- For a data type 'Tree', will generate the function name 'skipTree'
skipFName :: Name -> Name
skipFName tyName = mkName $ "skip" ++ sanitizeConName tyName

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
            [|case_|]
            lambdas
    let fun = FunD fName [Clause [] (NormalB funExpr) []]
    signature <- genSkipSignature tyName
    return [signature, fun]

-- Generates all the lambda functions we will need, to skip using caseTree
genSkipLambdas :: [PackingFlag] -> Name -> Q [Exp]
genSkipLambdas flags tyName = do
    branchTypes <- getBranchesTyList tyName flags
    genSkipLambda `mapM` branchTypes

-- generates a single lambda to use with caseTree for our skip method
genSkipLambda :: [Type] -> Q Exp
genSkipLambda types = go types [|R.return ()|]
  where
    go [] end = end
    go [_] end = [|skip R.>> $end|]
    go (t1 : t2 : ts) end =
        if typeIsFieldSize t1
            then [|skipWithFieldSize R.>> $(go ts end)|]
            else [|skip R.>> $(go (t2 : ts) end)|]

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
