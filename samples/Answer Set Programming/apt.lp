% implementation of APT

#include "asp/define.lp".
#include "asp/generate.lp".
#include "asp/hard.lp".

:~ type(E,quantitative), channel(E,x), priority(E,P). [1@P,E]
:~ type(E,quantitative), channel(E,y), priority(E,P). [1@P,E]
:~ type(E,quantitative), channel(E,size), priority(E,P). [2@P,E]
:~ type(E,quantitative), channel(E,color), priority(E,P). [3@P,E]

:~ type(E,ordinal), channel(E,x), priority(E,P). [1@P,E]
:~ type(E,ordinal), channel(E,y), priority(E,P). [1@P,E]
:~ type(E,ordinal), channel(E,color), priority(E,P). [2@P,E]
:~ type(E,ordinal), channel(E,size), priority(E,P). [3@P,E]

:~ type(E,nominal), channel(E,x), priority(E,P). [1@P,E]
:~ type(E,nominal), channel(E,y), priority(E,P). [1@P,E]
:~ type(E,nominal), channel(E,color), priority(E,P). [2@P,E]
:~ type(E,nominal), channel(E,shape), priority(E,P). [3@P,E]
:~ type(E,nominal), channel(E,size), priority(E,P). [4@P,E]

:- channel(_,text).
:- channel(_,detail).
:- channel(_,row).
:- channel(_,column).

% don't bin, use log, or aggregate
:- bin(_).
:- log(_).
:- aggregate(_,_).

#show mark/1.
#show channel/2.
