%% Copyright (c) 2008-2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : lfe_scan.xrl
%% Author  : Robert Virding
%% Purpose : Token definitions for Lisp Flavoured Erlang.

Definitions.
B    = [01]
O    = [0-7]
D    = [0-9]
H    = [0-9a-fA-F]
B36  = [0-9a-zA-Z]
U    = [A-Z]
L    = [a-z]
A    = ({U}|{L})
DEL  = [][()}{";\000-\s]
SYM  = [^][()}{";\000-\s\177-\237]
SSYM = [^][()}{"|;#`',\000-\s\177-\237]
WS   = ([\000-\s]|;[^\n]*)

Rules.
%% Bracketed Comments using #| foo |#
#{D}*\|[^\|]*\|+([^#\|][^\|]*\|+)*# :
        block_comment(string:substr(TokenChars, 3)).

%% Separators
'               :    {token,{'\'',TokenLine}}.
`               :    {token,{'`',TokenLine}}.
,               :    {token,{',',TokenLine}}.
,@              :    {token,{',@',TokenLine}}.
\.              :    {token,{'.',TokenLine}}.
[][()}{]        :    {token,{list_to_atom(TokenChars),TokenLine}}.

#{D}*[bB]\(     :    {token,{'#B(',TokenLine}}.
#{D}*[mM]\(     :    {token,{'#M(',TokenLine}}.
#{D}*\(         :    {token,{'#(',TokenLine}}.
#{D}*\.         :    {token,{'#.',TokenLine}}.

#{D}*`          :    {token,{'#`',TokenLine}}.
#{D}*;          :    {token,{'#;',TokenLine}}.
#{D}*,          :    {token,{'#,',TokenLine}}.
#{D}*,@         :    {token,{'#,@',TokenLine}}.

%% Characters
#{D}*\\(x{H}+|.) :   char_token(skip_past(TokenChars, $\\, $\\), TokenLine).

%% Based numbers
#{D}*\*{SYM}+   :    base_token(skip_past(TokenChars, $*, $*), 2, TokenLine).
#{D}*[bB]{SYM}+ :    base_token(skip_past(TokenChars, $b, $B), 2, TokenLine).
#{D}*[oO]{SYM}+ :    base_token(skip_past(TokenChars, $o, $O), 8, TokenLine).
#{D}*[dD]{SYM}+ :    base_token(skip_past(TokenChars, $d, $D), 10, TokenLine).
#{D}*[xX]{SYM}+ :    base_token(skip_past(TokenChars, $x, $X), 16, TokenLine).
#{D}*[rR]{SYM}+ :
        %% Scan over digit chars to get base.
        {Base,[_|Ds]} = base1(tl(TokenChars), 10, 0),
        base_token(Ds, Base, TokenLine).

%% String
"(\\x{H}+;|\\.|[^"\\])*" :
        %% Strip quotes.
        S = string:substr(TokenChars, 2, TokenLen - 2),
        {token,{string,TokenLine,chars(S)}}.
%% Binary string
#"(\\x{H}+;|\\.|[^"\\])*" :
        %% Strip quotes.
        S = string:substr(TokenChars, 3, TokenLen - 3),
        Bin = unicode:characters_to_binary(chars(S), utf8, utf8),
        {token,{binary,TokenLine,Bin}}.
%% Symbols
\|(\\x{H}+;|\\.|[^|\\])*\| :
        %% Strip quotes.
        S = string:substr(TokenChars, 2, TokenLen - 2),
        symbol_token(chars(S), TokenLine).
%% Funs
#'{SSYM}{SYM}*/{D}+ :
        %% Strip sharpsign single-quote.
        FunStr = string:substr(TokenChars,3),
        {token,{'#\'',TokenLine,FunStr}}.
%% Atoms
[+-]?{D}+       :
        case catch {ok,list_to_integer(TokenChars)} of
            {ok,I} -> {token,{number,TokenLine,I}};
            _ -> {error,"illegal integer"}
        end.
[+-]?{D}+\.{D}+([eE][+-]?{D}+)? :
        case catch {ok,list_to_float(TokenChars)} of
            {ok,F} -> {token,{number,TokenLine,F}};
            _ -> {error,"illegal float"}
        end.
{SSYM}{SYM}*    :
        symbol_token(TokenChars, TokenLine).
{WS}+           :    skip_token.

Erlang code.
%% Copyright (c) 2008-2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : lfe_scan.erl
%% Author  : Robert Virding
%% Purpose : Token definitions for Lisp Flavoured Erlang.

-export([start_symbol_char/1,symbol_char/1]).

-import(string, [substr/2,substr/3]).

%% start_symbol_char(Char) -> true | false.
%% symbol_char(Char) -> true | false.
%%  Define start symbol chars and symbol chars.

start_symbol_char($#) -> false;
start_symbol_char($`) -> false;
start_symbol_char($') -> false;                 %'
start_symbol_char($,) -> false;
start_symbol_char($|) -> false;                 %Symbol quote character
start_symbol_char(C) -> symbol_char(C).

symbol_char($() -> false;
symbol_char($)) -> false;
symbol_char($[) -> false;
symbol_char($]) -> false;
symbol_char(${) -> false;
symbol_char($}) -> false;
symbol_char($") -> false;
symbol_char($;) -> false;
symbol_char(C) -> ((C > $\s) and (C =< $~)) orelse (C > $\240).

%% symbol_token(Chars, Line) -> {token,{symbol,Line,Symbol}} | {error,E}.
%%  Build a symbol from list of legal characters, else error.

symbol_token(Cs, L) ->
    case catch {ok,list_to_atom(Cs)} of
        {ok,S} -> {token,{symbol,L,S}};
        _ -> {error,"illegal symbol"}
    end.

%% base_token(Chars, Base, Line) -> Integer.
%%  Convert a string of Base characters into a number. We only allow
%%  base betqeen 2 and 36, and an optional sign character first.

base_token(_, B, _) when B < 2; B > 36 ->
    {error,"illegal number base"};
base_token([$+|Cs], B, L) -> base_token(Cs, B, +1, L);
base_token([$-|Cs], B, L) -> base_token(Cs, B, -1, L);
base_token(Cs, B, L) -> base_token(Cs, B, +1, L).

base_token(Cs, B, S, L) ->
    case base1(Cs, B, 0) of
        {N,[]} -> {token,{number,L,S*N}};
        {_,_} -> {error,"illegal based number"}
    end.

base1([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $a, C =< $z, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $A, C =< $Z, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base1(Cs, Base, Next);
base1([C|Cs], _Base, SoFar) -> {SoFar,[C|Cs]};
base1([], _Base, N) -> {N,[]}.

-define(IS_UNICODE(C), ((C >= 0) and (C =< 16#10FFFF))).

%% char_token(InputChars, Line) -> {token,{number,L,N}} | {error,E}.
%%  Convert an input string into the corresponding character. For a
%%  sequence of hex characters we check resultant is code is in the
%%  unicode range.

char_token([$x,C|Cs], L) ->
    case base1([C|Cs], 16, 0) of
        {N,[]} when ?IS_UNICODE(N) -> {token,{number,L,N}};
        _ -> {error,"illegal character"}
    end;
char_token([C], L) -> {token,{number,L,C}}.

%% chars(InputChars) -> Chars.
%%  Convert an input string into the corresponding string characters.
%%  We know that the input string is correct.

chars([$\\,$x,C|Cs0]) ->
    case hex_char(C) of
        true ->
            case base1([C|Cs0], 16, 0) of
                {N,[$;|Cs1]} -> [N|chars(Cs1)];
                _Other -> [escape_char($x)|chars([C|Cs0])]
            end;
        false -> [escape_char($x)|chars([C|Cs0])]
    end;
chars([$\\,C|Cs]) -> [escape_char(C)|chars(Cs)];
chars([C|Cs]) -> [C|chars(Cs)];
chars([]) -> [].

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

escape_char($b) -> $\b;                %\b = BS
escape_char($t) -> $\t;                %\t = TAB
escape_char($n) -> $\n;                %\n = LF
escape_char($v) -> $\v;                %\v = VT
escape_char($f) -> $\f;                %\f = FF
escape_char($r) -> $\r;                %\r = CR
escape_char($e) -> $\e;                %\e = ESC
escape_char($s) -> $\s;                %\s = SPC
escape_char($d) -> $\d;                %\d = DEL
escape_char(C) -> C.

%% Block Comment:
%%  Provide a sensible error when people attempt to include nested
%%  comments because currently the parser cannot process them without
%%  a rebuild. But simply exploding on a '#|' is not going to be that
%%  helpful.

block_comment(TokenChars) ->
    %% Check we're not opening another comment block.
    case string:str(TokenChars, "#|") of
        0 -> skip_token; %% No nesting found
        _ -> {error, "illegal nested block comment"}
    end.

%% skip_until(String, Char1, Char2) -> String.
%% skip_past(String, Char1, Char2) -> String.

%% skip_until([C|_]=Cs, C1, C2) when C =:= C1 ; C =:= C2 -> Cs;
%% skip_until([_|Cs], C1, C2) -> skip_until(Cs, C1, C2);
%% skip_until([], _, _) -> [].

skip_past([C|Cs], C1, C2) when C =:= C1 ; C =:= C2 -> Cs;
skip_past([_|Cs], C1, C2) -> skip_past(Cs, C1, C2);
skip_past([], _, _) -> [].
