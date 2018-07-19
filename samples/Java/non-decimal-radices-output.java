public static void main(String args[]){
   for(int a= 0;a < 33;a++){
      System.out.println(Integer.toBinaryString(a));
      System.out.println(Integer.toOctalString(a));
      System.out.println(Integer.toHexString(a));
      //the above methods treat the integer as unsigned
      //there are also corresponding Long.to***String() methods for long's.

      System.out.printf("%3o %2d %2x\n",a ,a ,a); //printf like the other languages; binary not supported
   }
}
