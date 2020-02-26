private predicate foo(F f){
  f = f
}

int predicateWithResult(){
  result = 43
}

class A extends int {
  A() {
    this = -1
  }
}

select (A)-1
