function millis() { // Gets current time in milliseconds.
  return (new Date()).getTime();
}

/* Executes function 'func' n times, returns array of execution times. */
function benchmark(n, func, args) {
  var times = [];
  for (var i=0; i<n; i++) {
    var m = millis();
    func.apply(func, args);
    times.push(millis() - m);
  }
  return times;
}
