{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple

main = do
     db <- open "postal.db"
     execute_ db "\
     \CREATE TABLE address (\
        \addrID     INTEGER PRIMARY KEY AUTOINCREMENT, \
        \addrStreet TEXT NOT NULL, \
        \addrCity   TEXT NOT NULL, \
        \addrState  TEXT NOT NULL, \
        \addrZIP    TEXT NOT NULL  \
     \)"
     close db
