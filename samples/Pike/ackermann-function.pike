int main(){
   write(ackermann(3,4) + "\n");
}

int ackermann(int m, int n){
   if(m == 0){
      return n + 1;
   } else if(n == 0){
      return ackermann(m-1, 1);
   } else {
      return ackermann(m-1, ackermann(m, n-1));
   }
}
