program Cuboid_Demo(output);

procedure DoCuboid(sWidth, sHeight, Depth: integer);
  const
    widthScale  = 4;
    heightScale = 3;
  type
    TPage = array of array of char;
  var
    Cuboid: TPage;
    i, j: integer;
    Width, Height: integer;
    totalWidth, totalHeight: integer;
  begin
    Width  := widthScale  * sWidth;
    Height := heightScale * sHeight;
    totalWidth  := 2 * Width + Depth + 3;
    totalHeight := Height + Depth + 3;
    setlength (Cuboid, totalHeight + 1);
    for i := 1 to totalHeight do
      setlength (Cuboid[i], totalwidth + 1);
    // points
    for i := low(Cuboid) to high(Cuboid) do
      for j := low(Cuboid[i]) to high(Cuboid[i]) do
        Cuboid[i,j] := ' ';
    Cuboid [1, 1]                      := '+';
    Cuboid [Height + 2, 1]             := '+';
    Cuboid [1, 2 * Width + 2]          := '+';
    Cuboid [Height + 2, 2 * Width + 2] := '+';
    Cuboid [totalHeight, Depth + 2]    := '+';
    Cuboid [Depth + 2, totalWidth]     := '+';
    Cuboid [totalHeight, totalWidth]   := '+';
    // width lines
    for I := 1 to 2 * Width do
    begin
       Cuboid [1, I + 1]                   := '-';
       Cuboid [Height + 2, I + 1]          := '-';
       Cuboid [totalHeight, Depth + I + 2] := '-';
    end;
    // height lines
    for I := 1 to Height do
    begin
       Cuboid [I + 1, 1]                  := '|';
       Cuboid [I + 1, 2 * Width + 2]      := '|';
       Cuboid [Depth + I + 2, totalWidth] := '|';
    end;
    // depth lines
    for I := 1 to Depth do
    begin
       Cuboid [Height + 2 + I, 1 + I]             := '/';
       Cuboid [1 + I, 2 * Width + 2 + I]          := '/';
       Cuboid [Height + 2 + I, 2 * Width + 2 + I] := '/';
    end;
    for i := high(Cuboid) downto 1 do
    begin
      for j := 1 to high(Cuboid[i]) do
        write (Cuboid[i,j]);
      writeln;
    end;
  end;

begin
  writeln('1, 1, 1:');
  DoCuboid(1, 1, 1);
  writeln('2, 3, 4:');
  DoCuboid(2, 3, 4);
  writeln('6, 2, 1:');
  DoCuboid(6, 2, 1);
end.
