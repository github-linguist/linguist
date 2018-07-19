require 'rubygems'
require 'net/ldap'
ldap = Net::LDAP.new(:host => 'ldap.example.com', :base => 'o=companyname')
ldap.authenticate('bind_dn', 'bind_pass')
