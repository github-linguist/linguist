Note 'ttt adjudicates or plays'

   use:  markers ttt characters

   characters represent the cell names.

   markers is a length 3 character vector of the
   characters to use for first and second player
   followed by the opponent's mark.
   'XOX' means X plays 1st, O is the other mark,
   and the first strategy plays 1st.
   'XOO' means X plays 1st, O is the other mark,
   and the second strategy moves first.

   The game  is set  up for  the computer as the
   first strategy (random), and human as second.

   A standard use:
      'XOX'ttt'abcdefghijkl'

   Example game reformatted w/ emacs artist-mode
   to fit your display:

      '#-#'ttt'wersdfxcv'
   w│e│r                     w│e│r        ....    -│e│r           .    -│e│#
   ─┼─┼─                 .   ─┼─┼─       ..       ─┼─┼─         ..     ─┼─┼─
   s│d│f                 .   s│#│f      ..        s│#│f        ..      -│#│f
   ─┼─┼─                ..   ─┼─┼─      .         ─┼─┼─      ...       ─┼─┼─
   x│c│v               ..    -│c│v      .         -│c│#     ..         -│c│#
   d                  ..     v         ..         r         .          VICTORY
   w│e│r             ..      w│e│r     ..         -│e│#     .
   ─┼─┼─           ...       ─┼─┼─     ..         ─┼─┼─    .
   s│#│f         ...         s│#│f     ..         s│#│f    .
   ─┼─┼─        ..           ─┼─┼─   ...          ─┼─┼─  ...
   x│c│v                     -│c│#                -│c│#
   -->cell for -?            -->cell for -?       -->cell for -?
   x                         w                    s
)

while=: conjunction def 'u ^: v ^:_' NB. j assumes while is a verb and needs to know while is a conjunction.

ttt=: outcome@:((display@:move) while undecided)@:display@:prepare

blindfolded_variant=: outcome@:display@:(move while undecided)@:display@:prepare

outcome=: {&(>;:'kitty VICTORY')@:won   NB. (outcome does not pass along the state)
move=: post locate
undecided=: won nor full
prepare=: , board@:]                    NB. form the state vector

Note 'locate'
  is a monadic verb.  y is the state vector.
  returns the character of the chosen cell.
  Generally:
  locate=: second_strategy`first_strategy@.(first = mark)
  Simplified:
  locate=: human_locate NB. human versus human
)
locate=: human_locate`computer_locate@.(first = mark)

display=: show [: (1 1,:5 5)&(];.0)@:": [: <"0 fold

computer_locate=: [: show@{. board -. marks NB. strategy: first available
computer_locate=: [: show@({~ ?@:#) board -. marks NB. strategy: random

human_locate=: monad define
  state=. y
  m=. mark state
  b=. board state
  cells=. b -. marks state
  show '-->cell for ' , m , '?'
  whilst. cell -.@:e. cells do. cell =. {. (1!:1]1) , m end.
)

post=: 2&A.@:(3&{.)@:[ prepare mark@:[`((i.~ board)~)`(board@:[)}

mark=: {.                    NB. mark of the current player from state
marks=: 2&{.                 NB. extract both markers from state
board=: _9&{.                NB. extract board from state
first=: 2&{                  NB. extract first player from state

show=: [ smoutput

full=: 2 = #@:~.
won=: test@:fold
fold=: 3 3 $ board
test=: [: any [: all [: infix_pairs_agree |:@:lines

lines=: , diagonal , diagonal@:|. , |:
diagonal=: (<0 1)&|:
all=: *./
any=: +./
nor=: 8 b.
infix_pairs_agree=: 2&(=/\)
