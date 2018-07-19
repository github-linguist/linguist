package Inheritance is
   type Animal is tagged private;
   type Dog is new Animal with private;
   type Cat is new Animal with private;
   type Lab is new Dog with private;
   type Collie is new Dog with private;
private
   type Animal is tagged null record;
   type Dog is new Animal with null record;
   type Cat is new Animal with null record;
   type Lab is new Dog with null record;
   type Collie is new Dog with null record;
end Inheritance;
