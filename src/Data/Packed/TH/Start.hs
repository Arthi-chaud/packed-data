module Data.Packed.TH.Start (startFName, genStart) where

import Data.Packed.Needs
import Data.Packed.Packable (write)
import Data.Packed.TH.Flag (PackingFlag (..))
import Data.Packed.TH.Utils
import Data.Word (Word8)
import Language.Haskell.TH

-- | For a constructor 'Leaf', will generate the function name 'startLeaf'
startFName :: Name -> Name
startFName conName = mkName $ "start" ++ sanitizeConName conName

-- | Generates a function that prepares a 'Data.Packed.Needs' to receive values from a data constructor.
--
-- __Example:__
--
-- For the 'Tree' data type, it generates the following functions
--
-- @
-- startLeaf :: NeedsBuilder (Tree a ': r) t (a ': r) t
-- startLeaf = 'mkNeedsBuilder' (\n -> runBuilder (write (0 :: Word8) ('unsafeCastNeeds' n)))
--
-- startNode :: NeedsBuilder (Tree a ': r) t (Tree a ': Tree a ': r) t
-- startNode = 'mkNeedsBuilder' (\n -> runBuilder (write (1 :: Word8) ('unsafeCastNeeds' n)))
-- @
genStart ::
    [PackingFlag] ->
    -- | Constructor to generate the function for
    Con ->
    -- | The 'Tag' (byte) to write for this constructor
    Tag ->
    Q [Dec]
genStart flags con tag = do
    branchType <- getBranchTyList con flags
    let (conName, _) = getNameAndBangTypesFromCon con
        fName = startFName conName
    (DataConI _ conType _) <- reify conName
    sig <-
        let r = varT $ mkName "r"
            t = varT $ mkName "t"
            destNeedsTypeParams = foldr (\field rest -> [t|$(return field) ': $rest|]) r branchType
            parentType = getParentTypeFromConstructorType conType
         in [t|NeedsBuilder ($(return parentType) ': $r) $t $destNeedsTypeParams $t|]
    expr <- [|mkNeedsBuilder (\n -> runBuilder (write (tag :: Word8)) (unsafeCastNeeds n))|]
    return
        [ SigD fName sig
        , FunD fName [Clause [] (NormalB expr) []]
        ]
