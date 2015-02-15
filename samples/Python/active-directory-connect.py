import ldap

l = ldap.initialize("ldap://ldap.example.com")
try:
    l.protocol_version = ldap.VERSION3
    l.set_option(ldap.OPT_REFERRALS, 0)

    bind = l.simple_bind_s("me@example.com", "password")
finally:
    l.unbind()
