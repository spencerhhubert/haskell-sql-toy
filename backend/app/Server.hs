{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.Either (fromRight)
import Web.Scotty.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, open, execute, Only, execute_, query_, fromOnly)
import qualified Database.SQLite.Simple as SQL
import Data.Text.Lazy (Text, pack, unpack)
import Sql_parser (parseSQL, parseJSONFile, SpecialList, applySQL)
import Data.Aeson (encode)
import Network.Wai.Middleware.Cors (simpleCors)

data Config = Config
    {
        port :: Int,
        db_conn :: Connection,
        parsed_json :: SpecialList
    }

--adds read only access to app state within the monad that is IO
--this is what info can be passed between different monad transformers
type ConfigM = ReaderT Config IO
--Scott application that can throw errors of type Text and operations on the monad configm
--this is the Scotty specific monad
type ScottyM = ScottyT Text ConfigM
--sqlite doesn't need a monad transformer because its info lives in config

main :: IO ()
main = do
    --first arg is db path, second is json to be parsed and sql'd
    args <- getArgs
    let db_path = args !! 0
    let json_path = args !! 1
    conn <- open db_path
    execute_ conn "CREATE TABLE IF NOT EXISTS history (id INTEGER PRIMARY KEY, query TEXT)"
    raw <- parseJSONFile json_path
    json <- case raw of
        Left err -> error err
        Right val -> return val
    let config = Config { port=4000, db_conn=conn, parsed_json=json }
    scottyT (port config) (\m -> runReaderT m config) app

--app needs to be of type ScottyT error type main monad type
app :: ScottyM ()
app = do
    middleware simpleCors
    get "/" rootHandler
    get "/sql/" sqlHandler --should this be a post? looks annoying with parsing bytestrings
    get "/history/" historyHandler

rootHandler :: ActionT Text ConfigM () --ActionT that can throw errors of type Text and operations on the monad configm
rootHandler = do
    text "You don't have a robot?"

sqlHandler :: ActionT Text ConfigM ()
sqlHandler = do
    parsed_json <- lift $ asks parsed_json
    query <- param ("query" :: Text) :: ActionT Text ConfigM Text
    let parsed = parseSQL $ unpack query --show doesn't work, need to unpack. what is a unpack vs a show
    case parsed of
        Left err -> text $ pack err --returns useless errors
        Right val -> do
            conn <- lift $ asks db_conn
            --todo check for duplicates
            liftIO $ execute conn "INSERT INTO history (query) VALUES (?)" (SQL.Only query)
            let result = applySQL val parsed_json 
            json result

historyHandler :: ActionT Text ConfigM ()
historyHandler = do
    conn <- lift $ asks db_conn
    queries <- liftIO $ query_ conn "SELECT query FROM history" :: ActionT Text ConfigM [Only String]
    json $ removeDuplicates (map fromOnly queries) --should just do this before adding a new query

removeDuplicates :: [String] -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)


