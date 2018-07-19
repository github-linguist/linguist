function middleThree(x){
  var n=''+Math.abs(x); var l=n.length-1;
  if(l<2||l%2) throw new Error(x+': Invalid length '+(l+1));
  return n.slice(l/2-1,l/2+2);
}

[123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
1, 2, -1, -10, 2002, -2002, 0].forEach(function(n){
  try{console.log(n,middleThree(n))}catch(e){console.error(e.message)}
});
