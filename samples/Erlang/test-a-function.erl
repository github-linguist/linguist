-module( palindrome_tests ).
-compile( export_all ).
-include_lib( "eunit/include/eunit.hrl" ).

abcba_test() -> ?assert( palindrome:is_palindrome("abcba") ).

abcdef_test() -> ?assertNot( palindrome:is_palindrome("abcdef") ).
