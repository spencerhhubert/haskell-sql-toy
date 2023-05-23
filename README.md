# haskell-sql-toy

## challenge 1:
- make a sql parser for an array of flat json objects, as if they were a table
- this is implemented with attoparsec and aeson

## challenge 2:
- put that parser on a web server
- the server uses Scotty inside a ReaderT transformer stack to pass info between the parser, Scotty, and a SQLite database
- the front end uses Svelte

## how to run/use:
`chmod +x run.sh`
`./run.sh`
go to `localhost:3000` in the browser
