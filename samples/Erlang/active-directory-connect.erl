-module(ldap_example).
-export( [main/1] ).

main( [Host, DN, Password] ) ->
 {ok, Handle} = eldap:open( [Host] ),
 ok = eldap:simple_bind( Handle, DN, Password ),
 eldap:close( Handle ).
