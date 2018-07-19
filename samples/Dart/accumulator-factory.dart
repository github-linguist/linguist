Function accumulator(var n) => (var i) => n += i;

void main() {
  var a = accumulator(42);
  print("${a(0)}, ${a(1)}, ${a(10)}, ${a(100)}");

  var b = accumulator(4.2);
  print("${b(0)}, ${b(1)}, ${b(10.0)}, ${b(100.4)}");
}
