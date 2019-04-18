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
import PMD.SQLGen
import PMD.HEALMapping

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
                                SQLCreate tn cols) tables ++
                           [
                             SQLCreate "reviewer_organization" [("reviewer", SQLVarchar), ("organization", SQLVarchar)],
                             SQLCreate "name" [("table", SQLVarchar), ("column", SQLVarchar), ("index", SQLVarchar), ("id", SQLVarchar), ("description", SQLVarchar)]                
                           ] in
                    execSQLs db user pass sqls
   
