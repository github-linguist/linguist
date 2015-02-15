 public class Foo { public int x = 0; }

 void somefunction() {
     Foo a; // this declares a reference to Foo object; if this is a class field, it is initialized to null
     a = new Foo(); // this assigns a to point to a new Foo object
     Foo b = a; // this declares another reference to point to the same object that "a" points to
     a.x = 5; // this modifies the "x" field of the object pointed to by "a"
     System.out.println(b.x); // this prints 5, because "b" points to the same object as "a"
 }
