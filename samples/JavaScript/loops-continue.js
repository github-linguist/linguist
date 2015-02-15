var output = "";
for (var i = 1; i <= 10; i++) {
  output += i;
  if (i % 5 == 0) {
    print(output);
    output = "";
    continue;
  }
  output += ", ";
}
