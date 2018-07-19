-module (align_columns).

-export([align_left/0, align_right/0, align_center/0]).
-define (Lines,
         ["Given\$a\$text\$file\$of\$many\$lines\$where\$fields\$within\$a\$line\$",
          "are\$delineated\$by\$a\$single\$'dollar'\$character,\$write\$a\$program",
          "that\$aligns\$each\$column\$of\$fields\$by\$ensuring\$that\$words\$in\$each\$",
          "column\$are\$separated\$by\$at\$least\$one\$space.",
          "Further,\$allow\$for\$each\$word\$in\$a\$column\$to\$be\$either\$left\$",
          "justified,\$right\$justified,\$or\$center\$justified\$within\$its\$column."].

align_left()-> align_columns(left).
align_right()-> align_columns(right).
align_center()-> align_columns(centre).
align_columns(Alignment) ->
    Words = [ string:tokens(Line, "\$") || Line <- ?Lines ],
    Words_length  = lists:foldl( fun max_length/2, [], Words),
    Result = [prepare_line(Words_line, Words_length, Alignment)
              || Words_line <- Words],

    [ io:fwrite("~s~n", [lists:flatten(Line)]) || Line <- Result],
    ok.

max_length(Words_of_a_line, Acc_maxlength) ->
    Line_lengths = [length(W) || W <- Words_of_a_line ],
    Max_nb_of_length = lists:max([length(Acc_maxlength), length(Line_lengths)]),
    Line_lengths_prepared = adjust_list (Line_lengths, Max_nb_of_length, 0),
    Acc_maxlength_prepared = adjust_list(Acc_maxlength, Max_nb_of_length, 0),
    Two_lengths =lists:zip(Line_lengths_prepared, Acc_maxlength_prepared),
    [ lists:max([A, B]) || {A, B} <- Two_lengths].
adjust_list(L, Desired_length, Elem) ->
    L++lists:duplicate(Desired_length - length(L), Elem).

prepare_line(Words_line, Words_length, Alignment) ->
    All_words = adjust_list(Words_line, length(Words_length), ""),
    Zipped = lists:zip (All_words, Words_length),
    [ apply(string, Alignment, [Word, Length + 1, $\s])
      || {Word, Length} <- Zipped].

=== Output


1> c(align_columns).
{ok,align_columns}
2> align_columns:align_center().
   Given        a        text     file    of      many      lines     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.
ok
3> align_columns:align_left().
Given      a          text       file   of     many      lines      where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
ok
4> align_columns:align_right().
      Given          a       text   file     of      many      lines    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.
ok
