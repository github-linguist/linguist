Program Anagrams;

// assumes a local file

uses
  classes, math;

var
  i, j, k, maxCount: integer;
  sortedString:      string;
  WordList:          TStringList;
  SortedWordList:    TStringList;
  AnagramList:       array of TStringlist;

begin
  WordList := TStringList.Create;
  WordList.LoadFromFile('unixdict.txt');
  for i := 0 to WordList.Count - 1 do
  begin
    setLength(sortedString,Length(WordList.Strings[i]));
    sortedString[1] := WordList.Strings[i][1];

    // sorted assign
    j := 2;
    while j <=  Length(WordList.Strings[i]) do
    begin
      k := j - 1;
      while (WordList.Strings[i][j] < sortedString[k]) and (k > 0) do
      begin
        sortedString[k+1] := sortedString[k];
        k := k - 1;
      end;
      sortedString[k+1] :=  WordList.Strings[i][j];
      j := j + 1;
    end;

    // create the stringlists of the sorted letters and
    // the list of the original words
    if not assigned(SortedWordList) then
    begin
      SortedWordList := TStringList.Create;
      SortedWordList.append(sortedString);
      setlength(AnagramList,1);
      AnagramList[0] := TStringList.Create;
      AnagramList[0].append(WordList.Strings[i]);
    end
    else
    begin
      j := 0;
      while sortedString <> SortedWordList.Strings[j] do
      begin
        inc(j);
        if j = (SortedWordList.Count) then
        begin
          SortedWordList.append(sortedString);
          setlength(AnagramList,length(AnagramList) + 1);
          AnagramList[j] := TStringList.Create;
 	  break;
        end;
      end;
      AnagramList[j].append(WordList.Strings[i]);
    end;
  end;

  maxCount := 1;
  for i := 0 to length(AnagramList) - 1 do
    maxCount := max(maxCount, AnagramList[i].Count);

  // create output
  writeln('The largest sets of words have ', maxCount, ' members:');
  for i := 0 to length(AnagramList) - 1 do
  begin
    if AnagramList[i].Count = maxCount then
    begin
      write('"', SortedWordList.strings[i], '": ');
      for j := 0 to AnagramList[i].Count - 2 do
        write(AnagramList[i].strings[j], ', ');
      writeln(AnagramList[i].strings[AnagramList[i].Count - 1]);
    end;
  end;

  // Cleanup
  WordList.Destroy;
  SortedWordList.Destroy;
  for i := 0 to length(AnagramList) - 1 do
    AnagramList[i].Destroy;

end.
