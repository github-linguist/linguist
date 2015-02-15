function mersenne_factor(p){
  var limit, k, q
  limit = Math.sqrt(Math.pow(2,p) - 1)
  k = 1
  while ((2*k*p - 1) < limit){
    q = 2*k*p + 1
    if (isPrime(q) && (q % 8 == 1 || q % 8 == 7) && trial_factor(2,p,q)){
      return q // q is a factor of 2**p-1
    }
    k++
  }
  return null
}

function isPrime(value){
  for (var i=2; i < value; i++){
    if (value % i == 0){
      return false
    }
    if (value % i != 0){
      return true;
	 }
  }
}

function trial_factor(base, exp, mod){
  var square, bits
  square = 1
  bits = exp.toString(2).split('')
  for (var i=0,ln=bits.length; i<ln; i++){
    square = Math.pow(square, 2) * (bits[i] == 1 ? base : 1) % mod
  }
  return (square == 1)
}

function check_mersenne(p){
  var f, result
  console.log("M"+p+" = 2^"+p+"-1 is ")
  f = mersenne_factor(p)
  console.log(f == null ? "prime" : "composite with factor "+f)
}
