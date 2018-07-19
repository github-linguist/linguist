#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>

#include <mysql.h>
#include <openssl/md5.h>

void end_with_db(void);

MYSQL *mysql = NULL; // global...

bool connect_db(const char *host, const char *user, const char *pwd,
		const char *db, unsigned int port)
{
  if ( mysql == NULL )
  {
    if (mysql_library_init(0, NULL, NULL)) return false;
    mysql = mysql_init(NULL); if ( mysql == NULL ) return false;
    MYSQL *myp = mysql_real_connect(mysql, host, user, pwd, db, port, NULL, 0);
    if (myp == NULL) {
      fprintf(stderr, "connection error: %s\n", mysql_error(mysql));
      end_with_db();
      return false;
    }
  }
  return true; // already connected... ?
}

#define USERNAMELIMIT 32
// no part of the spec, but it is reasonable!!
#define PASSWORDLIMIT 32
#define SALTBYTE 16
bool create_user(const char *username, const char *password)
{
  int i;
  char binarysalt[SALTBYTE];
  char salt[SALTBYTE*2+1];
  char md5hash[MD5_DIGEST_LENGTH];
  char saltpass[SALTBYTE+PASSWORDLIMIT+1];
  char pass_md5[MD5_DIGEST_LENGTH*2 + 1];
  char user[USERNAMELIMIT*2 + 1];
  char *q = NULL;
  static const char query[] =
    "INSERT INTO users "
    "(username,pass_salt,pass_md5) "
    "VALUES ('%s', X'%s', X'%s')";
  static const size_t qlen = sizeof query;


  for(i=0; username[i] != '\0' && i < USERNAMELIMIT; i++) ;
  if ( username[i] != '\0' ) return false;
  for(i=0; password[i] != '\0' && i < PASSWORDLIMIT; i++) ;
  if ( password[i] != '\0' ) return false;

  srand(time(NULL));

  for(i=0; i < SALTBYTE; i++)
  {
    // this skews the distribution but it is lazyness-compliant;)
    binarysalt[i] = rand()%256;
  }

  (void)mysql_hex_string(salt, binarysalt, SALTBYTE);

  for(i=0; i < SALTBYTE; i++) saltpass[i] = binarysalt[i];
  strcpy(saltpass+SALTBYTE, password);
  (void)MD5(saltpass, SALTBYTE + strlen(password), md5hash);
  (void)mysql_hex_string(pass_md5, md5hash, MD5_DIGEST_LENGTH);

  (void)mysql_real_escape_string(mysql, user, username, strlen(username));

  // salt, pass_md5, user are db-query-ready
  q = malloc(qlen + USERNAMELIMIT*2 + MD5_DIGEST_LENGTH*2 + SALTBYTE*2 + 1);
  if ( q == NULL ) return false;
  sprintf(q, query, user, salt, pass_md5);
#if defined(DEBUG)
  fprintf(stderr, "QUERY:\n%s\n\n", q);
#endif
  int res = mysql_query(mysql, q);
  free(q);
  if ( res != 0 )
  {
    fprintf(stderr, "create_user query error: %s\n", mysql_error(mysql));
    return false;
  }
  return true;
}


bool authenticate_user(const char *username, const char *password)
{
  char user[USERNAMELIMIT*2 + 1];
  char md5hash[MD5_DIGEST_LENGTH];
  char saltpass[SALTBYTE+PASSWORDLIMIT+1];
  bool authok = false;
  char *q = NULL;
  int i;
  static const char query[] =
    "SELECT * FROM users WHERE username='%s'";
  static const size_t qlen = sizeof query;

  // can't be authenticated with invalid username or password
  for(i=0; username[i] != '\0' && i < USERNAMELIMIT; i++) ;
  if ( username[i] != '\0' ) return false;
  for(i=0; password[i] != '\0' && i < PASSWORDLIMIT; i++) ;
  if ( password[i] != '\0' ) return false;

  (void)mysql_real_escape_string(mysql, user, username, strlen(username));

  q = malloc(qlen + strlen(user) + 1);
  if (q == NULL) return false;
  sprintf(q, query, username);

  int res = mysql_query(mysql, q);
  free(q);
  if ( res != 0 )
  {
    fprintf(stderr, "authenticate_user query error: %s\n", mysql_error(mysql));
    return false;
  }

  MYSQL_RES *qr = mysql_store_result(mysql);
  if ( qr == NULL ) return false;

  // should be only a result, or none
  if ( mysql_num_rows(qr) != 1 ) {
    mysql_free_result(qr);
    return false;
  }

  MYSQL_ROW row = mysql_fetch_row(qr);          // 1 row must exist
  unsigned long *len = mysql_fetch_lengths(qr); // and should have 4 cols...

  memcpy(saltpass, row[2], len[2]); // len[2] should be SALTBYTE
  memcpy(saltpass + len[2], password, strlen(password));
  (void)MD5(saltpass, SALTBYTE + strlen(password), md5hash);

  authok = memcmp(md5hash, row[3], len[3]) == 0;
  mysql_free_result(qr);

  return authok;
}

void end_with_db(void)
{
  mysql_close(mysql); mysql = NULL;
  mysql_library_end();
}

int main(int argc, char **argv)
{

  if ( argc < 4 ) return EXIT_FAILURE;

  if ( connect_db("localhost", "devel", "", "test", 0 ) )
  {
    if ( strcmp(argv[1], "add") == 0 )
    {
      if (create_user(argv[2], argv[3]))
	printf("created\n");
    } else if ( strcmp(argv[1], "auth") == 0 ) {
      if (authenticate_user(argv[2], argv[3]))
	printf("authorized\n");
      else
	printf("access denied\n");
    } else {
      printf("unknown command\n");
    }
    end_with_db();
  }
  return EXIT_SUCCESS;
}
