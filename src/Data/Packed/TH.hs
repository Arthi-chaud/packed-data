-- | Module responsible for generating various functions to manipulate packed data.
--
-- __Note__: For each example, consider that the code is generated for the following type:
--
-- @
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- @
module Data.Packed.TH (
    -- * Entrypoint
    mkPacked,
    PackingFlag (..),

    -- * Generate Case function
    genCase,

    -- * Generate Packing function
    genPackableInstance,
    genConstructorPackers,
    genConstructorRepackers,
    genWrite,
    genConWrite,
    genStart,

    -- * Generate Unpacking function
    genUnpackableInstance,
    genRead,

    -- * Generate Skip function
    genSkippableInstance,
    genSkip,

    -- * Generate Transform function
    genTransform,

    -- * Misc
    Tag,
) where

import Data.Packed.TH.Case
import Data.Packed.TH.Flag
import Data.Packed.TH.PackCon (genConstructorPackers)
import Data.Packed.TH.Packable
import Data.Packed.TH.Read
import Data.Packed.TH.RepackCon
import Data.Packed.TH.Skip
import Data.Packed.TH.Skippable (genSkippableInstance)
import Data.Packed.TH.Start
import Data.Packed.TH.Transform (genTransform)
import Data.Packed.TH.Unpackable
import Data.Packed.TH.Utils
import Data.Packed.TH.Write
import Data.Packed.TH.WriteCon
import Language.Haskell.TH

-- | Generate the following for the given type
--
--   - A 'case' function (see 'genCase')
--
--   - An instance of 'Data.Packed.Packable' (see 'genPackableInstance')
--
--   - An instance of 'Data.Packed.Unpackable' (see 'genUnpackableInstance')
--
--   - An instance of 'Data.Packed.Skippable' (see 'genSkippableInstance')
--
--  __Example:__
--
--
-- @
--  $('mkPacked' ''Tree ['Data.Packed.TH.InsertFieldSize'])
-- @
mkPacked ::
    -- | The name of the type to generate the functions for
    Name ->
    -- | Generation customisation flags
    [PackingFlag] ->
    Q [Dec]
mkPacked tyName flags = do
    caseFunction <- genCase flags tyName
    packableInstance <- genPackableInstance flags tyName
    unpackableInstance <- genUnpackableInstance flags tyName
    constructorPackers <- genConstructorPackers flags tyName
    constructorRepackers <- genConstructorRepackers flags tyName
    skipInstance <- genSkippableInstance flags tyName
    transformFunction <- genTransform flags tyName
    return $
        caseFunction
            ++ packableInstance
            ++ unpackableInstance
            ++ constructorPackers
            ++ constructorRepackers
            ++ skipInstance
            ++ transformFunction
