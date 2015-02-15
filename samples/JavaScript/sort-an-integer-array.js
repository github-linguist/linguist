function int_arr(a, b) {
  return a - b;
}
var numbers = [20, 7, 65, 10, 3, 0, 8, -60];
numbers.sort(int_arr);
document.write(numbers);
