:- use_module(library(plunit)).

:- begin_tests(plunit_test_example).

test(true_succeeds) :-
  true.

test(fail_fails, [fail]) :-
  fail.  % Interchangeable with false/0.

:- end_tests(plunit_test_example).
