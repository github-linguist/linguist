% vdc( N, Base, Out )
% Out = the Van der Corput representation of N in given Base
vdc( 0, _, [] ).
vdc( N, Base, Out ) :-
    Nr is mod(N, Base),
    Nq is N // Base,
    vdc( Nq, Base, Tmp ),
    Out = [Nr|Tmp].

% Writes every element of a list to stdout; no newlines
write_list( [] ).
write_list( [H|T] ) :-
    write( H ),
    write_list( T ).

% Writes the Nth Van der Corput item.
print_vdc( N, Base ) :-
    vdc( N, Base, Lst ),
    write('0.'),
    write_list( Lst ).
print_vdc( N ) :-
    print_vdc( N, 2 ).

% Prints the first N+1 elements of the Van der Corput
% sequence, each to its own line
print_some( 0, _ ) :-
    write( '0.0' ).
print_some( N, Base ) :-
    M is N - 1,
    print_some( M, Base ),
    nl,
    print_vdc( N, Base ).
print_some( N ) :-
    print_some( N, 2 ).

test :-
   writeln('First 10 members in base 2:'),
   print_some( 9 ),
   nl,
   write('7th member in base 4 (stretch goal) => '),
   print_vdc( 7, 4 ).
