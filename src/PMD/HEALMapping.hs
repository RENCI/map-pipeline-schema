{-# LANGUAGE OverloadedStrings #-}

module PMD.HEALMapping where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.List (groupBy)
import PMD.SQLGen

data RandomizationFeature = FirstName | LastName | Name | Id | Email | PhoneNumber | LongTitle | ShortTitle | Index | Int Int Int | Float | MONTHDASHYY | None deriving (Eq, Show)

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
    isPrimaryKey :: !Bool,
    isForeignKey :: !Bool,
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

newtype BoolWrapper = BoolWrapper { getBool :: Bool }

instance FromJSON BoolWrapper where
    parseJSON = withText "bool" $ \t -> case t of
      "yes" -> pure (BoolWrapper True)
      "TRUE" -> pure (BoolWrapper True)
      "FALSE" -> pure (BoolWrapper True)
      "" -> pure (BoolWrapper False)
      n -> fail ("cannot convert to Bool " ++ unpack n)

instance FromJSON SQLType where
    parseJSON = withText "sqltype" $ \t -> case t of
      "int" -> return SQLInteger
      "float" -> pure SQLFloat
      "boolean" -> pure SQLBoolean
      "date" -> pure SQLDate
      n | T.take 4 n == "text" -> pure SQLVarchar
      n -> fail ("cannot convert to SQLType " ++ unpack n)

instance FromJSON RandomizationFeature where
    parseJSON = withText "randomizationfeature" $ \t -> case t of
      "firstname" -> pure FirstName
      "lastname" -> pure LastName
      "name" -> pure Name
      "id" -> pure Id
      "email" -> pure Email
      "phonenumber" -> pure PhoneNumber
      "shorttitle" -> pure ShortTitle
      "longtitle" -> pure LongTitle
      "index" -> pure Index
      "nat" -> pure (Int 0 maxBound)
      "month-yy" -> pure (MONTHDASHYY)
      "" -> pure None
      f -> fail (unpack f)


instance FromJSON Item where
    parseJSON = withObject "object" $ \m ->
        Item
            <$> m .: "Fieldname_CTMD"
            <*> m .: "Fieldname_redcap"
            <*> m .: "Data Type"
            <*> m .: "Randomization_feature"
            <*> m .: "Dropdown Options"
            <*> (getBool <$> m .: "Lookup Needed")
            <*> m .: "Lookup Information"
            <*> m .: "Algorithm"
            <*> m .: "Key"
            <*> (getBool <$> m .: "Primary")
            <*> (getBool <$> m .: "Foreign")
            <*> m .: "FK_tablename"
            <*> m .: "Cardinality (Table_CTMD--FK_tablename)"
            <*> m .: "Table_CTMD"
            <*> m .: "Table_phase1"
            <*> (getBool <$> m .: "NOT NULL")
            <*> m .: "Default Value"
            <*> m .: "Field Status"
            <*> m .: "Instrument"
            <*> m .: "Description"
            <*> m .: "Comments"
