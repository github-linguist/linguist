txt = """Given\$a\$txt\$file\$of\$many\$lines,\$where\$fields\$within\$a\$line\$
are\$delineated\$by\$a\$single\$'dollar'\$character,\$write\$a\$program
that\$aligns\$each\$column\$of\$fields\$by\$ensuring\$that\$words\$in\$each\$
column\$are\$separated\$by\$at\$least\$one\$space.
Further,\$allow\$for\$each\$word\$in\$a\$column\$to\$be\$either\$left\$
justified,\$right\$justified,\$or\$center\$justified\$within\$its\$column."""

# left/right/center justification of strings:
ljust(s, width) = s * " "^max(0, width - length(s))
rjust(s, width) = " "^max(0, width - length(s)) * s
function center(s, width)
  pad = width - length(s)
  if pad <= 0
    return s
  else
    pad2 = div(pad, 2)
    return " "^pad2 * s * " "^(pad - pad2)
  end
end

parts = [split(rstrip(line, '$'), '$') for line in split(txt, '\n')]

max_widths = zeros(Int, maximum(length, parts))
for line in parts
  for (i, word) in enumerate(line)
    max_widths[i] = max(max_widths[i], length(word))
  end
end
max_widths += 1 # separate cols by at least one space

for (label, justify) in (("Left", ljust), ("Right",rjust), ("Center",center))
  println(label, " column-aligned output:")
  for line in parts
    for (j, word) in enumerate(line)
      print(justify(word, max_widths[j]))
    end
    println()
  end
  println("-"^sum(max_widths))
end
