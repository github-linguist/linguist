#include <ldap.h>

char *name, *password;
...

LDAP *ld = ldap_init("ldap.somewhere.com", 389);
ldap_simple_bind_s(ld, name, password);

LDAPMessage **result;
ldap_search_s(ld, "dc=somewhere,dc=com", LDAP_SCOPE_SUBTREE,
	/* search for all persons whose names start with joe or shmoe */
	"(&(objectclass=person)(|(cn=joe*)(cn=shmoe*)))",
	NULL, /* return all attributes */
	0,  /* want both types and values of attrs */
	result); /* ldap will allocate room for return messages */

/* arduously do stuff here to result, with ldap_first_message(),
	ldap_parse_result(), etc. */

ldap_msgfree(*result);	/* free messages */
ldap_unbind(ld);	/* disconnect */
