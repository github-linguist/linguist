Program GuessTheNumber(input, output);

var
  number, guess: integer;

begin
  randomize;
  number := random(10) + 1;
  writeln ('I''m thinking of a number between 1 and 10, which you should guess.');
  write   ('Enter your guess: ');
  readln  (guess);
  while guess <> number do
  begin
    writeln ('Sorry, but your guess is wrong. Please try again.');
    write   ('Enter your new guess: ');
    readln  (guess);
  end;
  writeln ('You made an excellent guess. Thank you and have a nice day.');
end.
