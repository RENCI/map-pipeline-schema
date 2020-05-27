module Main where

import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy)
import Data.Aeson (eitherDecodeFileStrict)
import PMD.SQLGen
import PMD.HEALMapping
import System.Exit (exitWith, ExitCode(ExitFailure))

main :: IO ()
main = do
    [inputFile, outputFile] <- getArgs
    putStrLn ("input: " ++ inputFile)
    -- putStrLn ("output: " ++ outputFile)
    contents <- eitherDecodeFileStrict inputFile
    case contents of
      Left err -> do
        putStrLn ("cannot decode json " ++ err)
        exitWith (ExitFailure (-1))
      Right rowsL -> do
        let isSameTable a b = tableHeal a == tableHeal b
            tables = groupBy isSameTable rowsL
            sqls = map (\table ->
                    let tn = T.unpack (tableHeal (head table))
                        cols = map (\i -> (T.unpack (fieldNameHEAL i), dataType i)) table in
                        SQLCreate tn cols) tables ++
                           [
                             SQLCreate "reviewer_organization" [("reviewer", SQLVarchar), ("organization", SQLVarchar)],
                             SQLCreate "name" [("table", SQLVarchar), ("column", SQLVarchar), ("index", SQLVarchar), ("id", SQLVarchar), ("description", SQLVarchar)]                
                           ]
        withFile outputFile WriteMode $ \h ->
                  mapM_ (hPutStrLn h . ( ++ ";") . toSQL) sqls
                
   
