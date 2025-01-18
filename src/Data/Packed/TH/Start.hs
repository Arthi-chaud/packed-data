module Data.Packed.TH.Start (startFName, genStart) where

import Data.Packed.FieldSize
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
    -- | The name of the data constructor to generate the function for
    Name ->
    -- | The 'Tag' (byte) to write for this constructor
    Tag ->
    -- | The list of 'Type's of the data constructor's arguments
    [Type] ->
    Q [Dec]
genStart flags conName tag paramTypeList = do
    let fName = startFName conName
        constructorParamTypes = return <$> paramTypeList
    (DataConI _ conType _) <- reify conName
    sig <-
        let r = varT $ mkName "r"
            t = varT $ mkName "t"
            insertFieldSizes = InsertFieldSize `elem` flags
            skipLastFieldSize = SkipLastFieldSize `elem` flags
            -- From the list of the constructor's parameters, generate the correct type for 'Data.Packed.Needs'
            -- For Leaf a, we will obtain Needs (a ': r)
            -- For node, if size flag is enabled, we will get Needs (FieldSize ': Tree a ': FieldSize ': Tree a ': r)
            destNeedsTypeParams =
                foldr
                    ( \(i, x) xs ->
                        if insertFieldSizes && (not skipLastFieldSize || (skipLastFieldSize && i /= 1))
                            then [t|FieldSize ': $x ': $xs|]
                            else [t|$x ': $xs|]
                    )
                    r
                    $ zip (reverse [0 .. length constructorParamTypes]) constructorParamTypes
         in [t|NeedsBuilder ($(return $ getParentTypeFromConstructorType conType) ': $r) $t $destNeedsTypeParams $t|]
    expr <- [|mkNeedsBuilder (\n -> runBuilder (write (tag :: Word8)) (unsafeCastNeeds n))|]
    return
        [ SigD fName sig
        , FunD fName [Clause [] (NormalB expr) []]
        ]
