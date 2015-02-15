% push( ELEMENT, STACK, NEW )
% True if NEW is [ELEMENT|STACK]
push(ELEMENT,STACK,[ELEMENT|STACK]).

% pop( STACK, TOP, NEW )
% True if TOP and NEW are head and tail, respectively, of STACK
pop([TOP|STACK],TOP,STACK).

% empty( STACK )
% True if STACK is empty
empty([]).
