function is_perfect(n)
{
 var sum = 1, i, sqrt=Math.floor(Math.sqrt(n));
 for (i = sqrt-1; i>1; i--)
 {
  if (n % i == 0) {
   sum += i + n/i;
  }
 }
 if(n % sqrt == 0)
  sum += sqrt + (sqrt*sqrt == n ? 0 : n/sqrt);
 return sum === n;
}


var i;
for (i = 1; i < 10000; i++)
{
 if (is_perfect(i))
  print(i);
}
