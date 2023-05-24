#!/bin/sh
cd backend
cabal build .
cabal run . -- previous_requests.db ../$1
