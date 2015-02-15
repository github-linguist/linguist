RegExp regexp = new RegExp(r'\w+\!');

String capitalize(Match m) => '${m[0].substring(0, m[0].length-1).toUpperCase()}';

void main(){
  String hello = 'hello hello! world world!';
  String hellomodified = hello.replaceAllMapped(regexp, capitalize);
  print(hello);
  print(hellomodified);
}
