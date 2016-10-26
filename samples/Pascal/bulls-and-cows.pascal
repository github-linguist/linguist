Program BullCow;

{$mode objFPC}

uses Math, SysUtils;

type
   TFourDigit = array[1..4] of integer;

Procedure WriteFourDigit(fd: TFourDigit);
{ Write out a TFourDigit with no line break following. }
var
   i: integer;
begin
   for i := 1 to 4 do
   begin
      Write(fd[i]);
   end;
end;

Function WellFormed(Tentative: TFourDigit): Boolean;
{ Does the TFourDigit avoid repeating digits? }
var
   current, check: integer;
begin

   Result := True;

   for current := 1 to 4 do
   begin
      for check := current + 1 to 4 do
      begin
         if Tentative[check] = Tentative[current] then
         begin
            Result := False;
         end;
      end;
   end;

end;

Function MakeNumber(): TFourDigit;
{ Make a random TFourDigit, keeping trying until it is well-formed. }
var
   i: integer;
begin
   for i := 1 to 4 do
   begin
      Result[i] := RandomRange(1, 9);
   end;
   if not WellFormed(Result) then
   begin
      Result := MakeNumber();
   end;
end;

Function StrToFourDigit(s: string): TFourDigit;
{ Convert an (input) string to a TFourDigit. }
var
   i: integer;
begin
   for i := 1 to Length(s) do
   begin
      StrToFourDigit[i] := StrToInt(s[i]);
   end;
end;

Function Wins(Num, Guess: TFourDigit): Boolean;
{ Does the guess win? }
var
   i: integer;
begin
   Result := True;
   for i := 1 to 4 do
   begin
      if Num[i] <> Guess[i] then
      begin
         Result := False;
         Exit;
      end;
   end;
end;

Function GuessScore(Num, Guess: TFourDigit): string;
{ Represent the score of the current guess as a string. }
var
   i, j, bulls, cows: integer;
begin

   bulls := 0;
   cows := 0;

   { Count the cows and bulls. }
   for i := 1 to 4 do
   begin
      for j := 1 to 4 do
      begin
         if  (Num[i] = Guess[j]) then
         begin
            { If the indices are the same, that would be a bull. }
            if (i = j) then
            begin
               bulls := bulls + 1;
            end
            else
            begin
               cows := cows + 1;
            end;
         end;
      end;
   end;

   { Format the result as a sentence. }
   Result := IntToStr(bulls) + ' bulls, ' + IntToStr(cows) + ' cows.';

end;

Function GetGuess(): TFourDigit;
{ Get a well-formed user-supplied TFourDigit guess. }
var
   input: string;
begin

   WriteLn('Enter a guess:');
   ReadLn(input);

   { Must be 4 digits. }
   if Length(input) = 4 then
   begin

      Result := StrToFourDigit(input);

      if not WellFormed(Result) then
      begin
         WriteLn('Four unique digits, please.');
         Result := GetGuess();
      end;

   end
   else
   begin
      WriteLn('Please guess a four-digit number.');
      Result := GetGuess();
   end;

end;

var
   Num, Guess: TFourDigit;
   Turns: integer;
begin

   { Initialize the randymnity. }
   Randomize();

   { Make the secred number. }
   Num := MakeNumber();

   WriteLn('I have a secret number. Guess it!');

   Turns := 0;

   { Guess until the user gets it. }
   While True do
   begin

      Guess := GetGuess();

      { Count each guess as a turn. }
      Turns := Turns + 1;

      { If the user won, tell them and ditch. }
      if Wins(Num, Guess) then
      begin
         WriteLn('You won in ' + IntToStr(Turns) + ' tries.');
         Write('The number was ');
         WriteFourDigit(Num);
         WriteLn('!');
         Exit;
      end
      else { Otherwise, score it and get a new guess. }
      begin
         WriteLn(GuessScore(Num, Guess));
      end;

   end;

end.
