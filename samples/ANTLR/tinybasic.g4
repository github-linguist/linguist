/*
BSD License
Copyright (c) 2017, Tom Everett
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar tinybasic;

program
    : line*
    ;

line
   : number statement CR
   | statement CR
   ;

statement
   : 'PRINT' exprlist
   | 'IF' expression relop expression 'THEN'? statement
   | 'GOTO' number
   | 'INPUT' varlist
   | 'LET'? vara '=' expression
   | 'GOSUB' expression
   | 'RETURN'
   | 'CLEAR'
   | 'LIST'
   | 'RUN'
   | 'END'
   ;

exprlist
   : (STRING | expression) (',' (STRING | expression))*
   ;

varlist
   : vara (',' vara)*
   ;

expression
   : ('+' | '-' | 'ε')? term (('+' | '-') term)*
   ;

term
   : factor (('*' | '/') factor)*
   ;

factor
   : 
   vara
   | number
   ;

vara
    : VAR
    | STRING
    ;

number
   : DIGIT +
   ;

relop
   : ('<' ('>' | '=' | 'ε')?)
   | ('>' ('<' | '=' | 'ε')?)
   | '='
   | '+'
   | '-'
   ;


STRING
   : '"' ~ ["\r\n]* '"'
   ;


DIGIT
   : '0' .. '9'
   ;


VAR
   : 'A' .. 'Z'
   ;


CR
   : [\r\n]+
   ;


WS
   : [ \t] -> skip
   ;
