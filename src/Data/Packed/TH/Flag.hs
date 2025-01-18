module Data.Packed.TH.Flag (PackingFlag (..)) where

-- | Options for the generation process.
--
-- __Beware__: these options alter the signature and behaviour of the generated functions.
data PackingFlag
    = -- | When specified, each field in a packed data constructor will be preceded by a 'Data.Packed.TH.FieldSize',
      -- which indicates the size of the following packed value.
      --
      -- __Example__
      --
      -- As a consequence, for the following type, the `caseTree` function will have the following signature
      --
      -- @
      --
      -- caseTree ::
      --     ('Data.Packed.PackedReader' ('Data.Packed.FieldSize' ': a ': r) r b) ->
      --     ('Data.Packed.PackedReader' ('Data.Packed.FieldSize' ': Tree a ': 'Data.Packed.FieldSize' ': Tree a ': r) r b) ->
      --     'Data.Packed.PackedReader' (Tree a ': r) r b
      -- @
      InsertFieldSize
    | -- | This flag should be used in complement to 'InsertFieldSize'
      --
      -- If set, no 'Data.Packed.FieldSize' will be inserted before the last parameter of the data constructor.
      --
      -- __Example__
      --
      -- If this flag is set (along with 'InsertFieldSize'), for the following type,
      -- the `caseTree` function will have the following signature
      --
      -- @
      -- caseTree ::
      --     ('Data.Packed.PackedReader' (a ': r) r b) ->
      --     ('Data.Packed.PackedReader' ('Data.Packed.FieldSize' ': Tree a ': Tree a ': r) r b) ->
      --     'Data.Packed.PackedReader' (Tree a ': r) r b
      -- @
      SkipLastFieldSize
    deriving (Eq)
