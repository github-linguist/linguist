// helper function to pretty print an Iterable
String iterableToString(Iterable seq) {
  String str = "[";
  Iterator i = seq.iterator();
  while(i.hasNext()) {
    str += i.next();
    if(i.hasNext()) {
      str += ",";
    }
  }
  return str + "]";
}

main() {
  int limit = 1000;
  Set<int> sieve = new Set<int>();

  for(int i = 2; i <= limit; i++) {
    sieve.add(i);
  }
  for(int i = 2; i * i <= limit; i++) {
    if(sieve.contains(i)) {
      for(int j = i * i; j <= limit; j += i) {
        sieve.remove(j);
      }
    }
  }
  var sortedValues = new List<int>.from(sieve);
  sortedValues.sort((int arg1, int arg2) {
    return arg1.compareTo(arg2);
  });
  print(iterableToString(sortedValues));
  Expect.equals(168, sieve.length);
}
