% library(win_menu) compiled into win_menu 0.00 sec, 12,872 bytes
% library(swi_hooks) compiled into pce_swi_hooks 0.00 sec, 2,404 bytes
% The graphical front-end will be used for subsequent tracing
% c:/users/joel-seven/appdata/roaming/swi-prolog/pl.ini compiled 0.13 sec, 876,172 bytes
XPCE 6.6.66, July 2009 for Win32: NT,2000,XP
Copyright (C) 1993-2009 University of Amsterdam.
XPCE comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
The host-language is SWI-Prolog version 5.10.0

For HELP on prolog, please type help. or apropos(topic).
         on xpce, please type manpce.

1 ?- assert((f(A, B,C) :- format('~w~w~w~w~n', [A, C, C, B]))).
true.

2 ?- f('Rosetta', 'Code', ':').
Rosetta::Code
true.

3 ?-
