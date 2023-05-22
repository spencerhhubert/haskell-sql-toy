{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, open)
import Data.Text.Lazy (Text)

data Config = Config
    {
        port :: Int,
        db_conn :: Connection,
        parsed_json :: String
    }

--adds read only access to app state within the monad that is IO
--this is what info can be passed between different monad transformers
type ConfigM = ReaderT Config IO
--Scott application that can throw errors of type Text and operations on the monad HandlerM
--this is the Scotty specific monad
type ScottyM = ScottyT Text ConfigM
--sqlite doesn't need a monad transformer because its info lives in config

main :: IO ()
main = do
    conn <- open "previous_requests.db"
    let config = Config { port=3000, db_conn=conn, parsed_json="later" }
    scottyT (port config) (\m -> runReaderT m config) app

--app needs to be of type ScottyT error type main monad type
app :: ScottyM ()
app = do
    get "/" handler

handler :: ActionT Text ConfigM () --ActionT that can throw errors of type Text and operations on the monad HandlerM
handler = do
    text "Hello, world!"
