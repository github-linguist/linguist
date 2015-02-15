public static void main(String[] args){
   int a;
   //...input or change a here
   assert a == 42;//throws an AssertionError when a is not 42
   assert a == 42 : "Error message"; //throws an AssertionError
          //when a is not 42 with "Error message" for the message
          //the error message can be any non-void expression
}
