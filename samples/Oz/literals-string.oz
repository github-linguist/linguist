declare
Digit0 = &0       %% the character '0'
NewLine = &\n     %% a character with special representation
NewLine = &\012   %% characters can also be specified with octals

%% Strings are lists of characters, but can also be written in double quotes:
[&H &e &l &l &o] = "Hello"

AnAtom = 'Hello'         %% single quotes are used for atoms
Atom2 = hello = 'hello'  %% for atoms starting with a lower case letter, they are optional

%% To build strings out of other values, so-called virtual strings are used:
MyName = "Peter"
MyAge = 8
{System.showInfo MyName # " is " # MyAge # " years old."}
