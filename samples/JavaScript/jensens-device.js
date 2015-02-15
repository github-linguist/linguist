var obj;

function sum(o, lo, hi, term) {
  var tmp = 0;
  for (o.val = lo; o.val <= hi; o.val++)
    tmp += term();
  return tmp;
}

obj = {val: 0};
alert(sum(obj, 1, 100, function() {return 1 / obj.val}));
