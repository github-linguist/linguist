function mcpi(n){
  var x,y,m=0;

  for(var i = 0; i < n; i += 1) {
    x = Math.random();
    y = Math.random();

    if (x*x + y*y < 1) { m += 1; }
  }

  return 4*m/n;
}

console.log(mcpi(1000));
console.log(mcpi(10000));
console.log(mcpi(100000));
console.log(mcpi(1000000));
console.log(mcpi(10000000));
