function randomNormal() {
  return Math.cos(2 * Math.PI * Math.random()) * Math.sqrt(-2 * Math.log(Math.random()))
}

var a = []
for (var i=0; i < 1000; i++){
  a[i] = randomNormal() / 2 + 1
}
