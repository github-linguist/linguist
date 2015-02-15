String binary(int n) {
  if(n<0)
    throw new IllegalArgumentException("negative numbers require 2s complement");
  if(n==0) return "0";
  String res="";
  while(n>0) {
    res=(n%2).toString()+res;
    n=(n/2).toInt();
  }
  return res;
}

main() {
  print(binary(0));
  print(binary(1));
  print(binary(5));
  print(binary(10));
  print(binary(50));
  print(binary(9000));
  print(binary(65535));
  print(binary(0xaa5511ff));
  print(binary(0x123456789abcde));
  // fails due to precision limit
  print(binary(0x123456789abcdef));
}
