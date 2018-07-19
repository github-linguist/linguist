Program GuessIt(input, output);

var
  done, ok:        boolean;
  guess, upp, low: integer;
  res:             char;

begin
  writeln ('Choose a number between 0 and 1000.');
  write ('Press Enter and I will start to guess the number.');
  readln;
  upp := 1000;
  low := 0;
  repeat
    ok := false;
    guess := ( ( upp - low ) div 2 ) + low;
    write ('My guess is: ', guess:4);
    write ('. How did i score? ''l'' = too low, ''h'' = too high, ''c'' = correct : ');
    repeat
      readln (res);
      res := lowercase(res);
    until (res = 'c') or (res = 'l') or (res = 'h');
    case  res  of
      'l': low := guess;
      'h': upp := guess;
    else
      ok := true
    end;
  until ok;
  writeln ('So the number is: ', guess:4);
  writeln ('It was nice to play with you.');
end.
