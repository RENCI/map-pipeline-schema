module Main where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.Environment
import Data.Maybe (mapMaybe)
import Data.Char (isSpace, isNumber)
import Data.List (dropWhileEnd, intercalate)
import Debug.Trace
import System.IO
import Data.Csv (encode, ToField(..), decodeByName)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import HEALMapping


filterByName :: String -> [TagTree String] -> [TagTree String]
filterByName name tree = filter (\t -> case t of
    TagBranch n _ _ | n == name -> True
    _ -> False) tree

text :: TagTree String -> String
text (TagBranch _ _ ts) = dropWhileEnd isSpace (dropWhile isSpace (case (mapMaybe (\t ->
    case t of 
        TagLeaf (TagText td) -> Just td
        _ -> Nothing) ts) of
            [td] -> td
            td : tds -> 
                trace ("warning: extra text " ++ show tds) td
            _ -> 
                trace ("warning: empty text list") ""
            ))

text t = error (show t)

data DBType = SimpleType String 
            | EnumType String [(String, String, String)] deriving Show

instance ToField DBType where
    toField (SimpleType ty) = toField ty
    toField (EnumType ty choices) = toField (ty ++ "[" ++ intercalate "," (map (\(a, b, c) ->
                    "(" ++ a ++ "," ++ b ++ "," ++ c ++ ")") choices) ++ "]")

parseType :: TagTree String -> DBType
parseType t = 
    let tx = text t
        ty = takeWhile (`notElem` ", ") tx in
        case t of
            TagBranch _ _ ts ->
                case filterByName "table" ts of
                    [TagBranch _ _ table] | ty /= "sql" ->
                        let tbodys = filterByName "tbody" table in
                            case tbodys of
                                [] ->
                                    error ("emtpy table for type " ++ ty)
                                [TagBranch _ _ tbody] ->
                                    EnumType ty (map (\r ->
                                        case r of 
                                            TagBranch _ _ d -> 
                                                case filterByName "td" d of
                                                    [td1, td2] -> (text td1, "", text td2)
                                                    [td1, td2, td3] -> (text td1, text td2, text td3)
                                                    x -> error ("cannot parse enum for type " ++ ty ++ " " ++ show x)) (filterByName "tr" tbody))
                    _ -> SimpleType ty

main :: IO ()
main = do
    [inputFile, inputFile2, outputFile] <- getArgs
    content <- readFile inputFile
    let tree = parseTree content
        (TagBranch _ _ html) = head (filterByName "html" tree)
        (TagBranch _ _ body) = head (filterByName "body" html)
        (TagBranch _ _ table) = head (filterByName "table" body)
        (TagBranch _ _ tbody) = head (filterByName "tbody" table)
        rows = filter (\r -> length r == 4) (map (\r ->
            case r of 
                TagBranch _ _ d -> 
                     filterByName "td" d
                _ -> []) (filterByName "tr" tbody))
        rows2 = map (\r ->
            let [td1, td2, td3, td4] = r in
                (text td1, text td2, filter (`notElem` "\r\n") (renderTree [td3]), parseType td4)
            ) rows
        ctsa_name = map (\r ->
            let [td1, td2, td3, td4] = r
                column = text td2
                description = text td3 in
                ("org_name", drop 5 column, "", description)
            ) (filter (\r ->
                let [td1, td2, td3, td4] = r 
                    column = text td2 in
                    take 5 column == "ctsa_" && all isNumber (drop 5 column)
                ) rows)
        name = concatMap (\(_, column, _, td4) -> case td4 of
                            EnumType _ choices -> concatMap (\(td1, td2, td3) ->
                                                                    case td3 of
                                                                        "" -> []
                                                                        _ -> [(column, td1, td2, td3)]) choices 
                            SimpleType _ -> []) rows2 ++ ctsa_name
    withFile inputFile2 ReadMode $ \ h -> do
        contents <- BSL.hGetContents h
        case decodeByName contents of
            Left err -> putStrLn err
            Right (header, rows) ->
                let rowsL = V.toList rows
                    columnMap = M.fromList (map (\item -> (T.unpack (fieldNamePhase1 item), T.unpack (fieldNameHEAL item))) rowsL) in do
                    name2 <- concat <$> mapM (\(column, index, id, description) -> case M.lookup column columnMap of
                                    Nothing -> do
                                        putStrLn ("cannot find column " ++ column)
                                        return []
                                    Just columnHEAL -> return [(columnHEAL, index, id, description)]) name
                    BSL.writeFile outputFile (BSL8.pack "column,index,id,description\n" `BSL.append` encode name2)

    
