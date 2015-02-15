here=:0 :0
  0 :0 will be replaced by the text on the following lines.
  This is three tokens: two instances of the number 0 and
  one instance of the explicit definition token ':'.
  Any indentation in the here document will be retained in the result.
  There must be a space to the left of : or it will combine with the
  0 on its left to form the token 0: which is something completely
  different.

  The here document is terminated by a line which contains only a
  single right parenthesis ')' and optional white space.  In J's
  documentation the family of entities which include here documents
  (and verb definitions and so on) are called 'scripts'.

  When several scripts are referenced on the same line, they are used
  sequentially in an order determined by their appearance on the line.
  The leftmost 'script' reference gets the last script and the rightmost
  reference gets the first script.  But this is a rare usage.

  Typically, such values are assigned a name so that they can be
  used later.  However, they may also be discarded and/or ignored, in
  which case they are logically equivalent to multi-line comments.
)

and_here=:noun define
  'noun define' is an alternative and perhaps more "user friendly"
  way of declaring a here document.  It achieves the same thing as
  0 :0 and in fact 'noun' has the value 0 and 'define' has the value :0
  And, of course, there must be a space between the word 'noun' and
  the word 'define'.

  Other useful alternatives include verb (which has the value 3)
  and dyad (which has the value 4), and adverb (which has the value 1).
  In other words 'verb define' (if unquoted) would be replaced by a
  verb whose definition is provided in the following 'script'.
  However, all of these names are normal variables which can
  be declared to have different values by the developer.  And, of course,
  note that this mechanism is significantly more verbose than using
  the underlying 0 :0 mechanism directly.
)
