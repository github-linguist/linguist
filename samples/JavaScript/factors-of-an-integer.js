function factors(num)
{
 var
  n_factors = [],
  i;

 for (i = 1; i <= Math.floor(Math.sqrt(num)); i += 1)
  if (num % i === 0)
  {
   n_factors.push(i);
   if (num / i !== i)
    n_factors.push(num / i);
  }
 n_factors.sort(function(a, b){return a - b;});  // numeric sort
 return n_factors;
}

factors(45);  // [1,3,5,9,15,45]
factors(53);  // [1,53]
factors(64);  // [1,2,4,8,16,32,64]
