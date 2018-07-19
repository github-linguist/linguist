List<int> hailstone(int n) {
  if(n<=0) {
    throw new IllegalArgumentException("start value must be >=1)");
  }
  Queue<int> seq=new Queue<int>();
  seq.add(n);
  while(n!=1) {
    n=n%2==0?(n/2).toInt():3*n+1;
    seq.add(n);
  }
  return new List<int>.from(seq);
}

// apparently List is missing toString()
String iterableToString(Iterable seq) {
  String str="[";
  Iterator i=seq.iterator();
  while(i.hasNext()) {
    str+=i.next();
    if(i.hasNext()) {
      str+=",";
    }
  }
  return str+"]";
}

main() {
  for(int i=1;i<=10;i++) {
    print("h($i)="+iterableToString(hailstone(i)));
  }
  List<int> h27=hailstone(27);
  List<int> first4=h27.getRange(0,4);
  print("first 4 elements of h(27): "+iterableToString(first4));
  Expect.listEquals([27,82,41,124],first4);

  List<int> last4=h27.getRange(h27.length-4,4);
  print("last 4 elements of h(27): "+iterableToString(last4));
  Expect.listEquals([8,4,2,1],last4);

  print("length of sequence h(27): "+h27.length);
  Expect.equals(112,h27.length);

  int seq,max=0;
  for(int i=1;i<=100000;i++) {
    List<int> h=hailstone(i);
    if(h.length>max) {
      max=h.length;
      seq=i;
    }
  }
  print("up to 100000 the sequence h($seq) has the largest length ($max)");
}
