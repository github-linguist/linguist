int M(int n) => n==0?1:n-F(M(n-1));
int F(int n) => n==0?0:n-M(F(n-1));

main() {
  String f="",m="";
  for(int i=0;i<20;i++) {
    m+="${M(i)} ";
    f+="${F(i)} ";
  }
  print("M: $m");
  print("F: $f");
}
