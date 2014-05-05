:- module(symbols, [symbol/2, symbol_mf/2, write_symbol/1]).

:- multifile symbol_mf/2.

symbol(Thing, Symbol) :- symbol_mf(Thing, Symbol).

write_symbol(Thing) :- once(symbol(Thing, Symbol)), write(Symbol).

