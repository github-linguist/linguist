no_argument()
one_argument( Arg )
optional_arguments( Arg, [{opt1, Opt1}, {another_opt, Another}] )
variable_arguments( [Arg1, Arg2 | Rest] )
names_arguments([{name1, Arg1}, {another_name, Another}] )
% Statement context?
% First class context?
Result = obtain_result( Arg1 )
% No way to distinguish builtin/user functions
% Subroutines?
% Arguments are passed by reference, but you can not change them.
% Partial application is possible (a function returns a function that has one argument bound)
