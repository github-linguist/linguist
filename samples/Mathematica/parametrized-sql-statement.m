Needs["DatabaseLink`"];
conn=OpenSQLConnection[JDBC["ODBC(DSN)", "testdb"], "Username" -> "John", "Password" -> "JohnsPassword"];
SQLUpdate[conn,"players",{"name","score","active"},{"Smith, Steve", 42,"TRUE"},SQLColumn["jerseyNum"] = 99];
CloseSQLConnection[conn];
