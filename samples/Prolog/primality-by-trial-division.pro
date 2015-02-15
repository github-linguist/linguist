prime(2).
prime(N) :-
  between(3, inf, N),
  1 is N mod 2,             % odd
  M is floor(sqrt(N+1)),    % round-off paranoia
  Max is (M-1) // 2,        % integer division
  forall( between(1, Max, I), N mod (2*I+1) > 0 ).
