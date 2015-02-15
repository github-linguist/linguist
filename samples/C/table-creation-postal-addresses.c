#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>

const char *code =
"CREATE TABLE address (\n"
"       addrID		INTEGER PRIMARY KEY AUTOINCREMENT,\n"
"	addrStreet	TEXT NOT NULL,\n"
"	addrCity	TEXT NOT NULL,\n"
"	addrState	TEXT NOT NULL,\n"
"	addrZIP		TEXT NOT NULL)\n" ;

int main()
{
  sqlite3 *db = NULL;
  char *errmsg;

  if ( sqlite3_open("address.db", &db) == SQLITE_OK ) {
    if ( sqlite3_exec(db, code, NULL, NULL,  &errmsg) != SQLITE_OK ) {
      fprintf(stderr, errmsg);
      sqlite3_free(errmsg);
      sqlite3_close(db);
      exit(EXIT_FAILURE);
    }
    sqlite3_close(db);
  } else {
    fprintf(stderr, "cannot open db...\n");
    sqlite3_close(db);
    exit(EXIT_FAILURE);
  }
  return EXIT_SUCCESS;
}
