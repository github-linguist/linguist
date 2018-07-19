Anagrams := function(name)
  local f, p, L, line, word, words, swords, res, cur, r;
  words := [ ];
  swords := [ ];
  f := InputTextFile(name);
  while true do
    line := ReadLine(f);
    if line = fail then
      break;
    else
      word := Chomp(line);
      Add(words, word);
      Add(swords, SortedList(word));
    fi;
  od;
  CloseStream(f);
  p := SortingPerm(swords);
  L := Permuted(words, p);
  r := "";
  cur := [ ];
  res := [ ];
  for word in L do
    if SortedList(word) = r then
      Add(cur, word);
    else
      if Length(cur) > 0 then
        Add(res, cur);
      fi;
      r := SortedList(word);
      cur := [ word ];
    fi;
  od;
  if Length(cur) > 0 then
    Add(res, cur);
  fi;
  return Filtered(res, v -> Length(v) > 1);
end;


ana := Anagrams("my/gap/unixdict.txt");;

# What is the longest anagram sequence ?
Maximum(List(ana, Length));
# 5

# Which are they ?
Filtered(ana, v -> Length(v) = 5);
# [ [ "abel", "able", "bale", "bela", "elba" ],
#   [ "caret", "carte", "cater", "crate", "trace" ],
#   [ "angel", "angle", "galen", "glean", "lange" ],
#   [ "alger", "glare", "lager", "large", "regal" ],
#   [ "elan", "lane", "lean", "lena", "neal" ],
#   [ "evil", "levi", "live", "veil", "vile" ] ]
