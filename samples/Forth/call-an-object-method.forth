include lib/compare.4th
include 4pp/lib/foos.4pp

[ASSERT]                               \ enable assertions

:: Cat
   class
     method: dynamicCat                \ virtual method
   end-class {

    :static staticCat { 2 } ;          \ static method
    :method { s" Mew!" } ; defines dynamicCat
  }                                    \ for unrelated classes,
;                                      \ method names have to differ

:: Dog
   class
     method: dynamicDog                \ virtual method
   end-class {

    :static staticDog { 5 } ;
    :method { s" Woof!" } ; defines dynamicDog
  }                                    \ for unrelated classes,
;                                      \ method names have to differ

static Cat c                           \ create two static objects
static Dog d

: main
  assert( class -> staticCat 2 = )     \ check for valid method return
  assert( class -> staticDog 5 = )     \ of a static method

  assert( c -> staticCat 2 = )         \ check for valid method return
  assert( d -> staticDog 5 = )         \ of a static method

  assert( c => dynamicCat s" Mew!"  compare 0= )
  assert( d => dynamicDog s" Woof!" compare 0= )
;                                      \ same for dynamic methods

main
