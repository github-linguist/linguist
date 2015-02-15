compress[uncompressed_] :=
  Module[{dictsize, dictionary, w, result, wc},
   dictsize = 256;
   dictionary = # -> # & /@ FromCharacterCode /@ Range@dictsize;
   w = "";
   result = {};
   Do[wc = w <> c;
    If[MemberQ[dictionary[[All, 1]], wc],
     w = wc,
     AppendTo[result, w /. dictionary];
     AppendTo[dictionary, wc -> dictsize];
     dictsize++;
     w = c],
    {c, Characters[uncompressed]}];
   AppendTo[result, w /. dictionary];
   result];
decompress::bc = "Bad compressed `1`";
decompress[compressed_] :=
  Module[{dictsize, dictionary, w, result, entry},
   dictsize = 256;
   dictionary = # -> # & /@ FromCharacterCode /@ Range@dictsize;
   w = result = compressed[[1]];
   Do[Which[MemberQ[dictionary[[All, 1]], k],
     entry = k /. dictionary,
     k == dictsize,
     entry = w <> StringTake[w, 1],
     True,
     Message[decompress::bc, k]];
    result = result <> entry;
    AppendTo[dictionary, dictsize -> w <> StringTake[entry, 1]];
    dictsize++;
    w = entry,
    {k, compressed[[2 ;;]]}];
   result];
(*How to use:*)
compress["TOBEORNOTTOBEORTOBEORNOT"]
decompress[%]
