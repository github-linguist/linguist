int main(int argc, array(int) argv){
   if(argc != 3){
      write("usage: `pike compare-two-ints.pike <x> <y>` where x and y are integers.\n");
      return 0;
   }

   int a = argv[1];
   int b = argv[2];

   if(a > b) {
      write(a + " is greater than " + b + "\n");
   } else if (a < b) {
      write(a + " is less than " + b + "\n");
   } else {
      write(a + " is equal to " + b + "\n");
   }
}
