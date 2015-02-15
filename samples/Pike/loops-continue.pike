int main(){
   for(int i = 1; i <= 10; i++){
      write(sprintf("%d",i));
      if(i % 5 == 0){
         write("\n");
         continue;
      }
      write(", ");
   }
}
