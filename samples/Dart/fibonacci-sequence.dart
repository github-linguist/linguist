int fib(int n) {
  if(n==0 || n==1) {
    return n;
  }
  int prev=1;
  int current=1;
  for(int i=2;i<n;i++) {
    int next=prev+current;
    prev=current;
    current=next;
  }
  return current;
}

int fibRec(int n) => n==0||n==1 ? n : fibRec(n-1)+fibRec(n-2);

main() {
  print(fib(11));
  print(fibRec(11));
}
