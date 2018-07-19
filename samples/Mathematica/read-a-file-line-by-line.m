strm=OpenRead["input.txt"];
If[strm=!=$Failed,
  While[line=!=EndOfFile,
    line=Read[strm];
    (*Do something*)
  ]];
Close[strm];
