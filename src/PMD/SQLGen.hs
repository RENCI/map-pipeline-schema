module PMD.SQLGen where

import Data.List (intercalate)
import Control.Exception
import Data.String

class ToSQL a where
    toSQL :: a -> String

data SQLType = SQLVarchar | SQLBoolean | SQLInteger | SQLDate | SQLFloat deriving (Eq, Show)

instance ToSQL SQLType where
    toSQL SQLVarchar = "varchar"
    toSQL SQLBoolean = "boolean"
    toSQL SQLInteger = "bigint"
    toSQL SQLDate = "date"
    toSQL SQLFloat = "real"

data SQLStatement = SQLCreate {
    tableName :: String,
    columns :: [(String, SQLType)]
}
    
sqlQuote :: String -> String
sqlQuote a = "\"" ++ a ++ "\""

instance ToSQL SQLStatement where
    toSQL (SQLCreate tn cols) = 
        "create table " ++ sqlQuote tn ++ " (" ++ intercalate ", " (map (\col ->
            sqlQuote (fst col) ++ " " ++ toSQL (snd col)) cols) ++ ")"


