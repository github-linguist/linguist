% Implemented by Arjun Sunel
string:left("Hello", length("Hello")-1,$.). 	% To strip the word from the right by 1

string:right("Hello", length("Hello")-1,$.).		% To strip the word from the left by 1

string:left(string:right("Hello", length("Hello")-1,$.), length("Hello")-2,$.).  %To strip the word from both sides by 1.
