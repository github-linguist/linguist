bottle = method(i,
  case(i,
    0, "no more bottles of beer",
    1, "1 bottle of beer",
    "#{i} bottles of beer"))

(99..1) each(i,
  "#{bottle(i)} on the wall, " println
  "take one down, pass it around," println
  "#{bottle(i - 1)} on the wall.\n" println
)
