function rangeExtraction(list) {
  var len = list.length;
  var out = [];
  var i, j;

  for (i = 0; i < len; i = j + 1) {
    // beginning of range or single
    out.push(list[i]);

    // find end of range
    for (var j = i + 1; j < len && list[j] == list[j-1] + 1; j++);
    j--;

    if (i == j) {
      // single number
      out.push(",");
    } else if (i + 1 == j) {
      // two numbers
      out.push(",", list[j], ",");
    } else {
      // range
      out.push("-", list[j], ",");
    }
  }
  out.pop(); // remove trailing comma
  return out.join("");
}

// using print function as supplied by Rhino standalone
print(rangeExtraction([
  0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
  37, 38, 39
]));
