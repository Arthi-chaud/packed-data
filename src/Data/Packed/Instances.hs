{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module provides instances of 'Data.Packed.Packable' and 'Data.Packed.Unpackable' for basic types like 'Prelude.List' and 'Prelude.Maybe'
module Data.Packed.Instances where

import Data.Packed.TH
import Data.Packed.Unpackable
import Prelude hiding (readList)

$(mkPacked ''[] [])
$(mkPacked ''Maybe [])
$(mkPacked ''Either [])
