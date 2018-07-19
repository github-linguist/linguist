TableCreation="CREATE TABLE address (	
addrID		INTEGER PRIMARY KEY AUTOINCREMENT,
addrStreet	TEXT NOT NULL,	addrCity	TEXT NOT NULL,
addrState	TEXT NOT NULL,	addrZIP		TEXT NOT NULL    )";

Needs["DatabaseLink`"]
conn=OpenSQLConnection[ JDBC[ "mysql","databases:1234/conn_test"], "Username" -> "test"]
SQLExecute[ conn, TableCreation]
