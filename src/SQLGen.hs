module SQLGen where

import Data.List (intercalate)
import Database.PostgreSQL.Simple
import Control.Exception
import Data.String

class ToSQL a where
    toSQL :: a -> String

data SQLType = SQLVarchar | SQLBoolean | SQLInteger | SQLDate deriving (Eq, Show)

instance ToSQL SQLType where
    toSQL SQLVarchar = "varchar"
    toSQL SQLBoolean = "boolean"
    toSQL SQLInteger = "integer"
    toSQL SQLDate = "date"

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

execSQLs :: String -> String -> String -> [SQLStatement] -> IO ()
execSQLs db user pass ss = bracket
    (connect defaultConnectInfo {
        connectUser = user,
        connectPassword = pass,
        connectDatabase = db
    })
    close
    (\c -> do
        mapM_ (\s -> execute_ c (fromString (toSQL s))) ss)


