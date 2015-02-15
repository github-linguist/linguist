InputTextFile("input.txt");
s := ReadAll(f);;  # two semicolons to hide the result, which may be long
CloseStream(f);
