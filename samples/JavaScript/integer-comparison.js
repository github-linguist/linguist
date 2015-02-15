// Using type coercion
function compare(a, b) {
  if (a==b) print(a + " equals " + b);
  if (a < b) print(a + " is less than " + b);
  if (a > b) print(a + " is greater than " + b);
}

// Without using type coercion and using standards
// Written for browsers
// assumption of a and b are both integers if typeof test passes
function compare (a, b) {
  if (typeof a === typeof b) {
    if (a === b) {
      document.writeln(a + " equals " + b);
    }
    if (a < b) {
      document.writeln(a + " is less than " + b);
    }
    if (a > b) {
      document.writeln(a + " is greater than " + b);
    }
  } else {
    // "1" and 1 are an example of this as the first is type string and the second is type number
    print(a + "{" + (typeof a) + "} and " + b + "{" + (typeof b) + "} are not of the same type 4and cannot be compared.");
  }
}
