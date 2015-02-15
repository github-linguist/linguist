package require sqlite3

sqlite3 db address.db
db eval {
    CREATE TABLE address (
	addrID		INTEGER PRIMARY KEY AUTOINCREMENT,
	addrStreet	TEXT NOT NULL,
	addrCity	TEXT NOT NULL,
	addrState	TEXT NOT NULL,
	addrZIP		TEXT NOT NULL
    )
}
