var hofstadterQ = function() {
   var memo = [1,1,1];
   var Q    = function (n) {
      var result = memo[n];
      if (typeof result !== 'number') {
         result  = Q(n - Q(n-1)) + Q(n - Q(n-2));
         memo[n] = result;
      }
      return result;
   };
   return Q;
}();

for (var i = 1; i <=10; i += 1) {
   console.log('Q('+ i +') = ' + hofstadterQ(i));
}

console.log('Q(1000) = ' + hofstadterQ(1000));
