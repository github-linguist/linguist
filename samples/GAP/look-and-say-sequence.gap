LookAndSay := function(s)
  local c, r, cur, ncur, v;
  v := "123";
  r := "";
  cur := 0;
  ncur := 0;
  for c in s do
    if c = cur then
      ncur := ncur + 1;
    else
      if ncur > 0 then
        Add(r, v[ncur]);
        Add(r, cur);
      fi;
      cur := c;
      ncur := 1;
    fi;
  od;
  Add(r, v[ncur]);
  Add(r, cur);
  return r;
end;

LookAndSay("1");     # "11"
LookAndSay(last);    # "21"
LookAndSay(last);    # "1211"
LookAndSay(last);    # "111221"
LookAndSay(last);    # "312211"
LookAndSay(last);    # "13112221"
LookAndSay(last);    # "1113213211"
LookAndSay(last);    # "31131211131221"
LookAndSay(last);    # "13211311123113112211"
LookAndSay(last);    # "11131221133112132113212221"
LookAndSay(last);    # "3113112221232112111312211312113211"
LookAndSay(last);    # "1321132132111213122112311311222113111221131221"
LookAndSay(last);    # "11131221131211131231121113112221121321132132211331222113112211"
LookAndSay(last);    # "311311222113111231131112132112311321322112111312211312111322212311322113212221"
