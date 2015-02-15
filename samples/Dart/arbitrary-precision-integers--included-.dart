void main() {
  var s = pow(5, pow(4, pow(3, 2))).toString();

  print('contains given digits: ${s.startsWith('62060698786608744707') && s.endsWith('92256259918212890625')}');
  print('number of digits: ${s.length}');
}
