even(N)  % in a body, suceeeds iff N is even.
odd(N).  % in a body, succeeds iff N is odd.

% rolling our own:
:- pred even(int::in) is semidet.

% It's an error to have all three in one module, mind; even/1 would fail to check as semidet.
even(N) :- N mod 2 = 0.   % using division that truncates towards -infinity
even(N) :- N rem 2 = 0.   % using division that truncates towards zero
even(N) :- N /\ 1 = 0.    % using bit-wise and.
