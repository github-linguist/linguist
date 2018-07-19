for (var i:int = 1; i <= 100; i++) {
  if (i % 15 == 0)
    trace('FizzBuzz');
  else if (i % 5 == 0)
    trace('Buzz');
  else if (i % 3 == 0)
    trace('Fizz');
  else
    trace(i);
}
