{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Csv
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy)
import SQLGen


data Item = Item {
    fieldNameHEAL :: !Text,
    filedNamePhase1 :: !Text,
    dataType :: !SQLType,
    lookupNeeded :: !Bool,
    algorithm :: !Text,
    key :: !Text,
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

instance FromNamedRecord Item where
    parseNamedRecord m =
        Item
            <$> m .: "Fieldname_HEAL"
            <*> m .: "Fieldname_phase1"
            <*> m .: "Data Type"
            <*> m .: "Lookup Needed"
            <*> m .: "Algorithm"
            <*> m .: "Key"
            <*> m .: "Table_HEAL"
            <*> m .: "Table_phase1"
            <*> m .: "NOT NULL"
            <*> m .: "Default Value"
            <*> m .: "Field Status"
            <*> m .: "Instrument"
            <*> m .: "Description"
            <*> m .: "Comments"

main :: IO ()
main = do
    [user, pass, db, inputFile] <- getArgs
    putStrLn ("input: " ++ inputFile)
    -- putStrLn ("output: " ++ outputFile)
    withFile inputFile ReadMode $ \ h -> do
        contents <- BSL.hGetContents h
        case decodeByName contents of
            Left err -> putStrLn err
            Right (header, rows) ->
                let rowsL = V.toList rows
                    isSameTable a b = tableHeal a == tableHeal b
                    tables = groupBy isSameTable rowsL
                    sqls = map (\table ->
                            let tn = T.unpack (tableHeal (head table))
                                cols = map (\i -> (T.unpack (fieldNameHEAL i), dataType i)) table in
                                SQLCreate tn cols) tables in
                    execSQLs user pass db sqls