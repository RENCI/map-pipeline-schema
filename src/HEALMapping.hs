{-# LANGUAGE OverloadedStrings #-}

module HEALMapping where

import Data.Csv
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy)
import SQLGen

data RandomizationFeature = FirstName | LastName | Name | Id | Email | PhoneNumber | LongTitle | ShortTitle | None

data Item = Item {
    fieldNameHEAL :: !Text,
    fieldNamePhase1 :: !Text,
    dataType :: !SQLType,
    randomizationFeature :: !RandomizationFeature,
    dropdownOptions :: !Text,
    lookupNeeded :: !Bool,
    lookupInformation :: !Text,
    algorithm :: !Text,
    key :: !Text,
    primary :: !Bool,
    foreign :: !Bool,
    foreignKeyTable :: !Text,
    cardinality :: !Text,
    tableHeal :: !Text,
    tablePhase1 :: !Text,
    nonNull :: !Bool,
    defaultValue :: !Text,
    fieldStatus :: !Text,
    instrument :: !Text,
    description :: !Text,
    comments :: !Text
} deriving (Eq, Show)

instance FromField Bool where
    parseField "yes" = pure True
    parseField "TRUE" = pure True
    parseField "FALSE" = pure True
    parseField "" = pure False
    parseField n = error ("cannot convert to Bool " ++ unpack n)

instance FromField SQLType where
    parseField "int" = pure SQLInteger
    parseField "boolean" = pure SQLBoolean
    parseField "date" = pure SQLDate
    parseField n | BS.take 4 n == "text" = pure SQLVarchar
    parseField n = error ("cannot convert to SQLType " ++ unpack n)

instance FromField RandomizationFeature where
    parseField "firstname" = FirstName
    parseField "lastname" = LastName
    parseField "name" = Name
    parseField "id" = Id
    parseField "email" = Email
    parseField "phonenumber" = PhoneNumber
    parseField "shorttitle" = ShortTitle
    parseField "longtitle" = LongTitle
    parseField "" = None


instance FromNamedRecord Item where
    parseNamedRecord m =
        Item
            <$> m .: "Fieldname_HEAL"
            <*> m .: "Fieldname_phase1"
            <*> m .: "Data Type"
            <*> m .: "Randomization_feature"
            <*> m .: "Dropdown Options"
            <*> m .: "Lookup Needed"
            <*> m .: "Lookup Information"
            <*> m .: "Algorithm"
            <*> m .: "Key"
            <*> m .: "Primary"
            <*> m .: "Foreign"
            <*> m .: "FK_tablename"
            <*> m .: "Cardinality (Table_HEAL--FK_tablename)"
            <*> m .: "Table_HEAL"
            <*> m .: "Table_phase1"
            <*> m .: "NOT NULL"
            <*> m .: "Default Value"
            <*> m .: "Field Status"
            <*> m .: "Instrument"
            <*> m .: "Description"
            <*> m .: "Comments"
