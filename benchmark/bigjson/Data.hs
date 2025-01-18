{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data (MBRelations (..), MBLifeSpan (..), MBArea (..), caseMBArea) where

import Data.Aeson
import Data.Packed
import GHC.Generics (Generic)

data MBUnknownObject = MBUnknownObject deriving (Generic)

$(mkPacked ''MBUnknownObject [])
instance FromJSON MBUnknownObject

data MBLifeSpan = MBLifeSpan
    { ended :: Bool
    , end :: Maybe String
    , begin :: Maybe String
    }
    deriving (Generic, Show)

$(mkPacked ''MBLifeSpan [InsertFieldSize])
instance FromJSON MBLifeSpan

data MBRelations = MBRelations
    { targetCredit :: String
    , relationBegin :: Maybe String
    , direction :: String
    , relationTypeId :: String
    , sourceCredit :: String
    , relationType :: String
    , targetType :: String
    , relationEnd :: Maybe String
    , relationEnded :: Bool
    }
    deriving (Generic, Show)

$(mkPacked ''MBRelations [InsertFieldSize])

instance FromJSON MBRelations where
    parseJSON = withObject "relation" $ \v ->
        MBRelations
            <$> v .: "target-credit"
            <*> v .: "begin"
            <*> v .: "direction"
            <*> v .: "type-id"
            <*> v .: "source-credit"
            <*> v .: "type"
            <*> v .: "target-type"
            <*> v .: "end"
            <*> v .: "ended"

data MBArea = MBArea
    { name :: String
    , areaType :: Maybe String
    , lifeSpan :: MBLifeSpan
    , relations :: [MBRelations]
    , id :: String
    , disambiguation :: String
    , sortName :: String
    , typeId :: Maybe String
    }
    deriving (Generic, Show)

$(mkPacked ''MBArea [InsertFieldSize])
instance FromJSON MBArea where
    parseJSON = withObject "Area" $ \v ->
        MBArea
            <$> v .: "name"
            <*> v .: "type"
            <*> v .: "life-span"
            <*> v .: "relations"
            <*> v .: "id"
            <*> v .: "disambiguation"
            <*> v .: "sort-name"
            <*> v .: "type-id"
