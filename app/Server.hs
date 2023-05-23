{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Web.Scotty.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, open)
import Data.Text.Lazy (Text)

import Sql_parser (parseSQL, parseJSONFile, SpecialList)

data Config = Config
    {
        port :: Int,
        db_conn :: Connection,
        parsed_json :: Either String SpecialList
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
    json <- parseJSONFile json_path
    let config = Config { port=3000, db_conn=conn, parsed_json=json }
    scottyT (port config) (\m -> runReaderT m config) app

--app needs to be of type ScottyT error type main monad type
app :: ScottyM ()
app = do
    get "/" handler
    post "/sql/:query" sqlHandler

handler :: ActionT Text ConfigM () --ActionT that can throw errors of type Text and operations on the monad configm
handler = do
    text "You don't have a robot?"

sqlHandler :: ActionT Text ConfigM ()
sqlHandler = do
    query <- param "query"
    let parsed = parseSQL $ show query --my sql parser is using strings, scotty seems easier with text type
    text query

