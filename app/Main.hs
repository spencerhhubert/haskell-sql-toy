import System.Environment
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.IO
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM

-- binary ops =, <, > between two values, one literal from the sql statement and one from the json, are conditions
-- AND and OR are conditions between conditions
-- need to parse (condition) AND/OR (another condition)
-- conditions are optional
-- ignore FROM statement
-- examples:
-- SELECT * WHERE id = 1;
-- SELECT * WHERE id = 1 AND name = "thing";
-- SELECT col1, col2;
--
--[{"id":1,"name":"thing","size": 1},{"id":2,"name":"thing2","size": 3.14}]
--

--calls that work
--SELECT *:
--SELECT * WHERE key = value;
--SELECT * WHERE ((key = value) AND ((key = value) OR (key = value)));
--SELECT col,col2 WHERE key = value;
--SELECT col, col2 WHERE key = value;
--etc.

type Op = String
type ColName = String
data Row = Row [(ColName, Value)] deriving (Show)
data Condition = BinaryOp Op Value ColName
                | AND Condition Condition
                | OR Condition Condition
                | Empty --return everything 
                deriving (Show)

evalOp :: Op -> Value -> Value -> Bool
evalOp ">" (Number a) (Number b) = b > a
evalOp "<" (Number a) (Number b) = b < a
evalOp "=" (Number a) (Number b) = a == b
evalOp "=" (String a) (String b) = a == b

evalCond :: Condition -> Row -> Bool
evalCond (BinaryOp op v1 c1) (Row ((c2, v2):xs)) = if c1 == c2 then evalOp op v1 v2 else evalCond (BinaryOp op v1 c1) (Row xs)
evalCond (AND cond1 cond2) row = (evalCond cond1 row) && (evalCond cond2 row)
evalCond (OR cond1 cond2) row = (evalCond cond1 row) || (evalCond cond2 row)
evalCond Empty _ = True
evalCond _ _ = False

--test
val1 = Number 1
val2 = Number 2
val3 = String $ T.pack "🐒"

row1 = Row [("col1", val1), ("col2", val2), ("col3", val3)]
cond1 = BinaryOp ">" val2 "col1"
eval1 = evalCond cond1 row1
--

evalStatement :: [ColName] -> Condition -> [Row] -> [Row]
evalStatement ["*"] cond rows = filter (evalCond cond) rows
evalStatement cols cond rows = map (Row . filter (\(c, v) -> c `elem` cols) . (\(Row xs) -> xs)) $ filter (evalCond cond) rows

parseLiteral :: Parser Value
parseLiteral = string <|> number
    where
        string = String . T.pack <$> many1 letter_ascii
        number = Number . read <$> many1 digit

ps s = stringCI $ pack s

condParser :: Parser Condition
condParser = nested <|> single
    where
        nested = do
            char '('
            cond1 <- condParser
            char ')'
            skipMany space
            comp <- ps "AND" <|> ps "OR"
            skipMany space
            char '('
            cond2 <- condParser
            char ')'
            return $ if comp == (pack "AND") then AND cond1 cond2 else OR cond1 cond2
        single = do
            col <- many1 letter_ascii
            skipMany space
            op <- ps "=" <|> ps ">" <|> ps "<"
            skipMany space
            val <- parseLiteral
            return $ BinaryOp (unpack op) val col

parseSelect :: Parser [ColName]
parseSelect = parseStar <|> parseCols
    where
        parseStar = do
            ps "SELECT"
            skipMany space
            ps "*"
            return ["*"]
        parseCols = do
            ps "SELECT"
            skipMany space
            cols <- many1 letter_ascii `sepBy1` (ps ", " <|> ps ",")
            return cols

sqlParser = do
    cols <- parseSelect
    skipMany space --skip the FROM statement because we're not doing real SQL
    cond <- option Empty (do
      _ <- stringCI $ pack "WHERE"
      skipMany space
      cond <- condParser
      return cond)
    char ';'
    return (cols, cond)

parseSQL :: String -> Either String ([ColName], Condition)
parseSQL = parseOnly sqlParser . pack

type SpecialMap = HM.HashMap T.Text Value
type SpecialList = [SpecialMap]

parseJSONFile :: FilePath -> IO (Either String SpecialList)
parseJSONFile path = fmap eitherDecode $ B.readFile path

--messed up defining data types, need some messy functions now
convertMap :: SpecialMap -> Row
convertMap = Row . map (\(k, v) -> (T.unpack k, v)) . HM.toList

convertList :: SpecialList -> [Row]
convertList = map convertMap

applySQL :: ([ColName], Condition) -> SpecialList -> [Row]
applySQL (cols, cond) = evalStatement cols cond . convertList

main :: IO ()
main = do
    args <- getArgs
    raw <- parseJSONFile $ head args
    json <- case raw of
        Left err -> error err
        Right json -> return json
    sql <- getLine
    let parsed = parseSQL sql
    case parsed of
        Left err -> error err
        Right sql -> print $ applySQL sql json
