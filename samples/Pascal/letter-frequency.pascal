program LetterFrequency;
var
  textFile: text;
  character: char;
  counter: array[0..255] of integer;
  i: integer;
begin
  for i := low(counter) to high(counter) do
    counter[i] := 0;
  assign(textFile, 'a_text_file.txt');
  reset(textFile);
  while not eof(textFile) do
  begin
    while not eoln(textFile) do
    begin
      read(textFile, character);
      inc(counter[ord(character)]);
    end;
    readln(textFile);
  end;
  for i := low(counter) to high(counter) do
    if counter[i] > 0 then
      writeln(char(i), ': ', counter[i]);
end.
