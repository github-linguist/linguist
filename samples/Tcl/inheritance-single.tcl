package require TclOO
oo::class create Animal {
   # ...
}
oo::class create Dog {
   superclass Animal
   # ...
}
oo::class create Cat {
   superclass Animal
   # ...
}
oo::class create Collie {
   superclass Dog
   # ...
}
oo::class create Lab {
   superclass Dog
   # ...
}
