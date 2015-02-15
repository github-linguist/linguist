for(int i = 1;i <= 10; i++){
   printf("%d", i);
   if(i % 5 == 0){
      printf("\n");
      continue;
   }
   printf(", ");
}
