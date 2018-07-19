package require ldap
set conn [ldap::connect $host $port]
ldap::bind $conn $user $password
