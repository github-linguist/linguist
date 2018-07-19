var Roman = {
  Values: [['M', 1000], ['CM', 900], ['D',  500], ['CD', 400],
           ['C',  100], ['XC',  90], ['L',  50],  ['XL',  40],
           ['X',   10], ['IX',   9], ['V',   5],  ['IV',   4],
           ['I',    1]],

  parse: function(str) {
    var result = 0
    for (var i=0; i<Roman.Values.length; ++i) {
      var pair = Roman.Values[i]
      var key = pair[0]
      var value = pair[1]
      var regex = RegExp('^' + key)
      while (str.match(regex)) {
        result += value
        str = str.replace(regex, '')
      }
    }
    return result
  }
}

var test_data = ['MCMXC', 'MDCLXVI', 'MMVIII']
for (var i=0; i<test_data.length; ++i) {
  var test_datum = test_data[i]
  print(test_datum + ": " + Roman.parse(test_datum))
}
