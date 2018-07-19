<?php

$l = ldap_connect('ldap.example.com');
ldap_set_option($l, LDAP_OPT_PROTOCOL_VERSION, 3);
ldap_set_option($l, LDAP_OPT_REFERRALS, false);

$bind = ldap_bind($l, 'me@example.com', 'password');

$base = 'dc=example, dc=com';
$criteria = '(&(objectClass=user)(sAMAccountName=username))';
$attributes = array('displayName', 'company');

$search = ldap_search($l, $base, $criteria, $attributes);
$entries = ldap_get_entries($l, $search);

var_dump($entries);
