use Net::LDAP;

my $ldap = Net::LDAP->new('ldap://ldap.example.com') or die $@;
my $mesg = $ldap->bind( $bind_dn, password => $bind_pass );
