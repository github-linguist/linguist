function dotProduct() {
  var len = arguments[0] && arguments[0].length;
  var argsLen = arguments.length;
  var i, j = len;
  var prod, sum = 0;

  // If no arguments supplied, return undefined
  if (!len) {
    return;
  }

  // If all vectors not same length, return undefined
  i = argsLen;
  while (i--) {

    if (arguments[i].length != len) {
      return;  // return undefined
    }
  }

  // Sum terms
  while (j--) {
    i = argsLen;
    prod = 1;

    while (i--) {
      prod *= arguments[i][j];
    }
    sum += prod;
  }
  return sum;
}

function crossProduct(a, b) {

  // Check lengths
  if (a.length != 3 || b.length != 3) {
     return;
  }

  return [a[1]*b[2] - a[2]*b[1],
          a[2]*b[0] - a[0]*b[2],
          a[0]*b[1] - a[1]*b[0]];

}

function scalarTripleProduct(a, b, c) {
  return dotProduct(a, crossProduct(b, c));
}

function vectorTripleProduct(a, b, c) {
  return crossProduct(a, crossProduct(b, c));
}

// Run tests
(function () {
  var a = [3, 4, 5];
  var b = [4, 3, 5];
  var c = [-5, -12, -13];

  alert(
    'A . B: ' + dotProduct(a, b) +
    '\n' +
    'A x B: ' + crossProduct(a, b) +
    '\n' +
    'A . (B x C): ' + scalarTripleProduct(a, b, c) +
    '\n' +
    'A x (B x C): ' + vectorTripleProduct(a, b, c)
  );
}());
