require 'rubygems'
require 'net/ldap'

ldap = Net::LDAP.new(:host => 'hostname', :base => 'base')
ldap.authenticate('bind_dn', 'bind_pass')

filter = Net::LDAP::Filter.pres('objectclass')
filter &= Net::LDAP::Filter.eq('sn','Jackman')
# or
filter = Net::LDAP::Filter.construct('(&(objectclass=*)(sn=Jackman))')

results = ldap.search(:filter => filter)  # returns an array of Net::LDAP::Entry objects

puts results[0][:sn]  # ==> "Jackman"
