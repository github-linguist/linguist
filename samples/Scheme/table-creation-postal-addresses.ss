(use sql-de-lite)

(define *db* (open-database "addresses"))

(exec ; create and run the SQL statement
  (sql *db*
       "CREATE TABLE address (
        addrID     INTEGER PRIMARY KEY AUTOINCREMENT,
        addrStreet TEXT NOT NULL,
        addrCity   TEXT NOT NULL,
        addrState  TEXT NOT NULL,
        addrZIP    TEXT NOT NULL
    )"
))

(close-database *db*) ; finally, close database
