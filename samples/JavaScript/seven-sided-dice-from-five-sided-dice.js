function dice5()
{
 return 1 + Math.floor(5 * Math.random());
}

function dice7()
{
 while (true)
 {
  var dice55 = 5 * dice5() + dice5() - 6;
  if (dice55 < 21)
   return dice55 % 7 + 1;
 }
}

distcheck(dice5, 1000000);
print();
distcheck(dice7, 1000000);
