main() {
  for(int i=1;i<=100;i++)
    print((i%3==0?"Fizz":"")+(i%5==0?"Buzz":"")+(i%3!=0&&i%5!=0?i:""));
}
