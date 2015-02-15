dog := new Collie
MsgBox, % "A " dog.__Class " is a " dog.base.base.__Class " and is part of the " dog.kingdom " kingdom."

class Animal {
   static kingdom := "Animalia" ; Class variable
}
class Dog extends Animal {
}
class Cat extends Animal {
}
class Lab extends Dog {
}
class Collie extends Dog {
}
