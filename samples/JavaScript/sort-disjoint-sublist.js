function sort_disjoint(values, indices) {
  var sublist = [];
  indices.sort(function(a, b) { return a > b; });

  for (var i = 0; i < indices.length; i += 1) {
    sublist.push(values[indices[i]]);
  }

  sublist.sort(function(a, b) { return a < b; });

  for (var i = 0; i < indices.length; i += 1) {
    values[indices[i]] = sublist.pop();
  }

  return values;
}
