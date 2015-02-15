for(int i = 1;i <= 10; i++){
   System.out.print(i);
   if(i % 5 == 0){
      System.out.println();
      continue;
   }
   System.out.print(", ");
}
