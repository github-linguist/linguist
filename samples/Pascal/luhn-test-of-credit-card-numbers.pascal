program luhn;

  function lunh(arg: string): boolean;
  var
    i, sum: integer;
    temp: byte;
  begin
    sum := 0;
    for i:= length(arg) downto 1 do begin  // Run the characters backwards
      temp := byte(arg[i])-48;             // Convert from ASCII to byte
      if (length(arg)-i) mod 2 = 0
        then sum := sum + temp             // Odd characters just add
        else if temp < 5
           then sum := sum + 2*temp        // Even characters add double
           else sum := sum + (2*temp)-9;   // or sum the digits of the doubling
    end;
    result := sum mod 10 = 0;              // Return true if sum ends in a 0
  end;

begin
  writeln('     49927398716: ', lunh('49927398716'));
  writeln('     49927398717: ', lunh('49927398717'));
  writeln('1234567812345678: ', lunh('1234567812345678'));
  writeln('1234567812345670: ', lunh('1234567812345670'));
end.
