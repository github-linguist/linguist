var soundex = function (s) {
     var a = s.toLowerCase().split('')
         f = a.shift(),
         r = '',
         codes = {
             a: '', e: '', i: '', o: '', u: '',
             b: 1, f: 1, p: 1, v: 1,
             c: 2, g: 2, j: 2, k: 2, q: 2, s: 2, x: 2, z: 2,
             d: 3, t: 3,
             l: 4,
             m: 5, n: 5,
             r: 6
         };

     r = f +
         a
         .map(function (v, i, a) { return codes[v] })
         .filter(function (v, i, a) { return ((i === 0) ? v !== codes[f] : v !== a[i - 1]); })
         .join('');

     return (r + '000').slice(0, 4).toUpperCase();
};

var tests = {
  "Soundex":     "S532",
  "Example":     "E251",
  "Sownteks":    "S532",
  "Ekzampul":    "E251",
  "Euler":       "E460",
  "Gauss":       "G200",
  "Hilbert":     "H416",
  "Knuth":       "K530",
  "Lloyd":       "L300",
  "Lukasiewicz": "L222",
  "Ellery":      "E460",
  "Ghosh":       "G200",
  "Heilbronn":   "H416",
  "Kant":        "K530",
  "Ladd":        "L300",
  "Lissajous":   "L222",
  "Wheaton":     "W350",
  "Ashcraft":    "A226",
  "Burroughs":   "B622",
  "Burrows":     "B620",
  "O'Hara":      "O600"
  };

for (var i in tests)
  if (tests.hasOwnProperty(i)) {
    console.log(
      i +
      '    \t' +
      tests[i] +
      '\t' +
      soundex(i) +
      '\t' +
      (soundex(i) === tests[i])
    );
}

// Soundex     S532  S532  true
// Example     E251  E251  true
// Sownteks    S532  S532  true
// Ekzampul    E251  E251  true
// Euler       E460  E460  true
// Gauss       G200  G200  true
// Hilbert     H416  H416  true
// Knuth       K530  K530  true
// Lloyd       L300  L300  true
// Lukasiewicz L222  L222  true
// Ellery      E460  E460  true
// Ghosh       G200  G200  true
// Heilbronn   H416  H416  true
// Kant        K530  K530  true
// Ladd        L300  L300  true
// Lissajous   L222  L222  true
// Wheaton     W350  W350  true
// Ashcraft    A226  A226  true
// Burroughs   B622  B622  true
// Burrows     B620  B620  true
// O'Hara      O600  O600  true
