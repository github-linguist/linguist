int main(){
   for(int i = 1; i <= 11; i++){
      write(sprintf("%d",i));
      if(i == 10){
         break;
      }
      write(", ");
   }
   write("\n");
}
