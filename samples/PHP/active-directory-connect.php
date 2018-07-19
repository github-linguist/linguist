<?php
$ldap = ldap_connect($hostname, $port);
$success = ldap_bind($ldap, $username, $password);
