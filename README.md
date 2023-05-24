# haskell-sql-toy

## challenge 1:
- make a sql parser for an array of flat json objects, as if they were a table
- this is implemented with attoparsec and aeson

## challenge 2:
- put that parser on a web server
- the server uses Scotty inside a ReaderT transformer stack to pass info between the parser, Scotty, and a SQLite database
- the front end uses Svelte

## how to run/use:
`chmod +x run_scotty.sh`

`chmod +x run_svelte.sh`

in one terminal, `./run_scotty.sh example.json` and another `./run_svelte.sh`

(preferably these would be in a docker compose but its being troublesome)

go to `https://localhost:3500`

![](https://raw.githubusercontent.com/spencerhhubert/haskell-sql-toy/main/assets/what_it_looks_like.png)
