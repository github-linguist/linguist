int main(){
   if(pal("rotator")){
      write("palindrome!\n");
   }
   if(!pal("asdf")){
      write("asdf isn't a palindrome.\n");
   }
}

int pal(string input){
   if( reverse(input) == input ){
      return 1;
   } else {
      return 0;
   }
}
