import Stdio;

int main(){
   if(exist("/var")){
      write("/var exists!\n");
   }

   if(exist("file-exists.pike")){
      write("I exist!\n");
   }
}
