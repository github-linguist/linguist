int A(int m, int n) => m==0 ? n+1 : n==0 ? A(m-1,1) : A(m-1,A(m,n-1));

main() {
  print(A(0,0));
  print(A(1,0));
  print(A(0,1));
  print(A(2,2));
  print(A(2,3));
  print(A(3,3));
  print(A(3,4));
  print(A(3,5));
  print(A(4,0));
}
