:- dynamic twiddles/2.
%_______________________________________________________________
% Arithemetic for complex numbers; only the needed rules
add(cx(R1,I1),cx(R2,I2),cx(R,I)) :- R is R1+R2, I is I1+I2.
sub(cx(R1,I1),cx(R2,I2),cx(R,I)) :- R is R1-R2, I is I1-I2.
mul(cx(R1,I1),cx(R2,I2),cx(R,I)) :- R is R1*R2-I1*I2, I is R1*I2+R2*I1.
polar_cx(Mag, Theta, cx(R, I)) :-     % Euler
	R is Mag * cos(Theta), I is Mag * sin(Theta).
%___________________________________________________
% FFT Implementation. Note: K rdiv N is a rational number,
% making the lookup in dynamic database predicate twiddles/2 very
% efficient.  Also, polar_cx/2 gets called only when necessary- in
% this case (N=8), exactly 3 times: (where Tf=1/4, 1/8, or 3/8).
tw(0,cx(1,0)) :- !.                    % Calculate e^(-2*pi*k/N)
tw(Tf, Cx) :- twiddles(Tf, Cx), !.     % dynamic match?
tw(Tf, Cx) :- polar_cx(1.0, -2*pi*Tf, Cx), assert(twiddles(Tf, Cx)).

fftVals(N, Even, Odd, V0, V1) :-       % solves all V0,V1 for N,Even,Odd
	nth0(K,Even,E), nth0(K,Odd,O), Tf is K rdiv N, tw(Tf,Cx),
	mul(Cx,O,M), add(E,M,V0), sub(E,M,V1).

split([],[],[]). % split [[a0,b0],[a1,b1],...] into [a0,a1,...] and [b0,b1,...]
split([[V0,V1]|T], [V0|T0], [V1|T1]) :- !, split(T, T0, T1).

fft([H], [H]).
fft([H|T], List) :-
	length([H|T],N),
	findall(Ve, (nth0(I,[H|T],Ve),I mod 2 =:= 0), EL), !, fft(EL, Even),
	findall(Vo, (nth0(I,T,Vo),I mod 2 =:= 0),OL), !, fft(OL, Odd),
	findall([V0,V1],fftVals(N,Even,Odd,V0,V1),FFTVals),    % calc FFT
	split(FFTVals,L0,L1), append(L0,L1,List).
%___________________________________________________
test :- D=[cx(1,0),cx(1,0),cx(1,0),cx(1,0),cx(0,0),cx(0,0),cx(0,0),cx(0,0)],
	time(fft(D,DRes)), writef('fft=['), P is 10^3, !,
	(member(cx(Ri,Ii), DRes), R is integer(Ri*P)/P, I is integer(Ii*P)/P,
	 write(R), (I>=0, write('+'),fail;write(I)), write('j, '),
	 fail; write(']'), nl).
