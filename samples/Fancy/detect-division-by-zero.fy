def divide: x by: y {
  try {
    x / y
  } catch DivisionByZeroError => e {
    e message println # prints error message
  }
}
