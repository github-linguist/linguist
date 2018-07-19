Module:   LZW
Synopsis: LZW implementation for Rosetta code

define method output(n :: <integer>)
  format-out("%d ", n);
end;

define method contains?(dict, var)
  let x = element(dict, var, default: #f);
  x ~= #f;
end;

define method byte->string(c)
  add("", as(<character>, c));
end;

define method compress(input :: <string>) => <vector>;
  let result = make(<vector>);
  let dict = make(<string-table>);
  for (x from 0 to 255)
    dict[byte->string(x)] := x;
  end;

  let next-code = 256;
  let cur-seq = "";
  for (c in input)
    let wc = add(cur-seq, c);
    if (contains?(dict, wc))
      cur-seq := wc;
    else
      result := add(result, dict[cur-seq]);
      dict[wc] := next-code;
      next-code := next-code + 1;
      cur-seq := add("", c);
    end
  end;
  unless (empty?(cur-seq))
    result := add(result, dict[cur-seq]);
  end;
  result
end;

format-out("%=\n", compress("TOBEORNOTTOBEORTOBEORNOT"))
