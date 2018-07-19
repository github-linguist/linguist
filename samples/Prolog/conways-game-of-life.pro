%----------------------------------------------------------------------%
% GAME OF LIFE                                                         %
%                                                                      %
% Adapt the prediacte grid_size according to the grid size of the      %
%   start pic.                                                         %
% Modify the number of generations.                                    %
% Run PROLOG and type '[gol].' to compile the source file.             %
% Create a subfolder <subfolder> where your gol.pl resides and place   %
%   your initial PBM '<filename>0.0000.pbm' inside <subfolder>.        %
% You need to adjust the number of zeros after <filename>. The         %
%   sequence of zeros after '0.' must be as long as the number of      %
%   generations. This is important to obtain a propper animation.      %
%   (Maybe someone knows a better solution for this)                   %
% Start PROLOG and run                                                 %
%                                                                      %
%                cellular('./<subloder>/<filename>').                  %
%                                                                      %
% Inside <subfolder> run the following shell command                   %
%                                                                      %
%         convert -delay 25 -loop 0 <filename>* <filename>.gif         %
%                                                                      %
%----------------------------------------------------------------------%

%----------------------------------------------------------------------%
% Special thanks to René Thiemann improving the runtime performance.   %
%----------------------------------------------------------------------%

% Size of the 2D grid
grid_size(300).
% Number of generations
generations(1000).

%----------------------------------------------------------------------%
% Main procedure: generate n generations of life and store each file.  %
%   cellular( +File path )                                             %
%----------------------------------------------------------------------%
cellular(I) :-
	grid_size(GS),
	string_concat(I,'0.0000.pbm',I1),
	read_pbm(I1,GS,M),
	cellular_(I,M,GS,1), !.

cellular_(I,M,GS,N) :-
	N1 is N+1,
	format(atom(N0),'~4d',N),
	string_concat(I,N0,I1),
	string_concat(I1,'.pbm',I2),
	step(M,M1),	
	write_pbm(M1,GS,I2), !,
	cellular_(I,M1,GS,N1).
cellular_(_,_,_,GE) :-
	generations(GE),!.

%----------------------------------------------------------------------%
% Apply the Game Of Life rule set to every cell.                       %
%   step( +OldMatrix, +NewMatrix )                                     %
%                                                                      %
%  ss | s | ... | s       ss ... step_ss                               %
% ----+---+-----+---      s  ... step_s                                %
%  ii | i | ... | i       ii ... step_ii                               %
% ----+---+-----+---      i  ... step_i                                %
%   : | : |  :  | :       ee ... step_ee                               %
% ----+---+-----+---      e  ... step_e                                %
%  ii | i | ... | i                                                    %
% ----+---+-----+---                                                   %
%  ee | e | ... | e                                                    %
%                                                                      %
%----------------------------------------------------------------------%
step([R1,R2|M],[H|T]) :-
	step_ss(R1,R2,H), !,
	step_([R1,R2|M],T).

step_([R1,R2,R3|M],[H|T]) :-
	step_ii(R1,R2,R3,H),
	step_([R2,R3|M],T), !.
step_([R1,R2],[H]) :-
	step_ee(R1,R2,H).

% Start case
step_ss([A1,A2|R1],[B1,B2|R2],[H|T]) :-
	rule([0,0,0],[0,A1,A2],[0,B1,B2],H),
	step_s([A1,A2|R1],[B1,B2|R2],T).
step_s([A1,A2,A3|R1],[B1,B2,B3|R2],[H|T]) :-	
	rule([0,0,0],[A1,A2,A3],[B1,B2,B3],H),
	step_s([A2,A3|R1],[B2,B3|R2],T).
step_s([A1,A2],[B1,B2],[H]) :-
	rule([0,0,0],[A1,A2,0],[B1,B2,0],H).

% Immediate case
step_ii([A1,A2|R1],[B1,B2|R2],[C1,C2|R3],[H|T]) :-
	rule([0,A1,A2],[0,B1,B2],[0,C1,C2],H),
	step_i([A1,A2|R1],[B1,B2|R2],[C1,C2|R3],T).
step_i([A1,A2,A3|R1],[B1,B2,B3|R2],[C1,C2,C3|R3],[H|T]) :-	
	rule([A1,A2,A3],[B1,B2,B3],[C1,C2,C3],H),
	step_i([A2,A3|R1],[B2,B3|R2],[C2,C3|R3],T).
step_i([A1,A2],[B1,B2],[C1,C2],[H]) :-
	rule([A1,A2,0],[B1,B2,0],[C1,C2,0],H).

% End case
step_ee([A1,A2|R1],[B1,B2|R2],[H|T]) :-
	rule([0,A1,A2],[0,B1,B2],[0,0,0],H),
	step_e([A1,A2|R1],[B1,B2|R2],T).
step_e([A1,A2,A3|R1],[B1,B2,B3|R2],[H|T]) :-	
	rule([A1,A2,A3],[B1,B2,B3],[0,0,0],H),
	step_e([A2,A3|R1],[B2,B3|R2],T).
step_e([A1,A2],[B1,B2],[H]) :-
	rule([A1,A2,0],[B1,B2,0],[0,0,0],H).

%----------------------------------------------------------------------%
% o Any dead cell with exactly three live neighbours becomes a live    %
%   cell, as if by reproduction.                                       %
% o Any other dead cell remains dead.                                  %
% o Any live cell with fewer than two live neighbours dies, as if      %
%   caused by under-population.                                        %
% o Any live cell with two or three live neighbours lives on to the    %
%   next generation.                                                   %
% o Any live cell with more than three live neighbours dies, as if by  %
%   overcrowding.                                                      %
%                                                                      %
% [Source: Wikipedia]                                                  %
%----------------------------------------------------------------------%
rule([A,B,C],[D,0,F],[G,H,I],1) :- A+B+C+D+F+G+H+I =:= 3.
rule([_,_,_],[_,0,_],[_,_,_],0).
rule([A,B,C],[D,1,F],[G,H,I],0) :- A+B+C+D+F+G+H+I < 2.
rule([A,B,C],[D,1,F],[G,H,I],1) :- A+B+C+D+F+G+H+I =:= 2.
rule([A,B,C],[D,1,F],[G,H,I],1) :- A+B+C+D+F+G+H+I =:= 3.
rule([A,B,C],[D,1,F],[G,H,I],0) :- A+B+C+D+F+G+H+I > 3.

%----------------------------------------------------------------------%
% Read a 2bit Protable Bitmap into a GS x GS 2-dimensional list.       %
%   read_pbm( +File path, +Grid size, -List 2D )                       %
%----------------------------------------------------------------------%
read_pbm(F,GS,M) :-
	open(F,read,S),
	skip(S,10),
	skip(S,10),
	get(S,C),
	read_file(S,C,L),
	nest(L,GS,M),
	close(S).

read_file(S,C,[CHR|T]) :-
	CHR is C-48,
	get(S,NC),
	read_file(S,NC,T).
read_file(_,-1,[]) :- !.

%----------------------------------------------------------------------%
% Morph simple list into a 2-dimensional one with size GS x GS         %
%   nest( ?List simple, ?Grid size, ?2D list )                         %
%----------------------------------------------------------------------%
nest(L,GS,[H|T]) :-
	length(H,GS),
	append(H,S,L),
	nest(S,GS,T).
nest(L,GS,[L]) :-
	length(L,S),
	S =< GS, !.	

%----------------------------------------------------------------------%
% Write a GS x GS 2-dimensional list into a 2bit Protable Bitmap.      %
%   write_pbm( +List 2D, +Grid size, +File path )                      %
%----------------------------------------------------------------------%	
write_pbm(L,GS,F) :-
	open(F,write,S),
	write(S,'P1'), nl(S),
	write(S,GS), put(S,' '), write(S,GS), nl(S),
	write_file(S,L),
	close(S).

write_file(S,[H|T]) :-
	write_line(S,H), nl(S),
	write_file(S,T).
write_file(_,[]) :- !.

write_line(S,[H|T]) :-
	write(S,H), put(S,' '),
	write_line(S,T).
write_line(_,[]) :- !.
