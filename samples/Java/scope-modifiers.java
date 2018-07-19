public //any class may access this member directly

protected //only this class, subclasses of this class,
//and classes in the same package may access this member directly

private //only this class may access this member directly

static //for use with other modifiers
//limits this member to one reference for the entire JVM

//adding no modifier (sometimes called "friendly") allows access to the member by classes in the same package

// Modifier    | Class | Package | Subclass | World
// ------------|-------|---------|----------|-------
// public      |  Y    |    Y    |    Y     |   Y
// protected   |  Y    |    Y    |    Y     |   N
// no modifier |  Y    |    Y    |    N     |   N
// private     |  Y    |    N    |    N     |   N

//method parameters are available inside the entire method

//Other declarations follow lexical scoping,
//being in the scope of the innermost set of braces ({}) to them.
//You may also create local scopes by surrounding blocks of code with braces.

public void function(int x){
   //can use x here
   int y;
   //can use x and y here
   {
      int z;
      //can use x, y, and z here
   }
   //can use x and y here, but NOT z
}
