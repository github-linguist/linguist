class Delegator {
  function __construct() {
    $this->delegate = NULL ;
  }
  function operation() {
    if(method_exists($this->delegate, "thing"))
      return $this->delegate->thing() ;
    return 'default implementation' ;
  }
}

class Delegate {
  function thing() {
    return 'Delegate Implementation' ;
  }
}

$a = new Delegator() ;
print "{$a->operation()}\n" ;

$a->delegate = 'A delegate may be any object' ;
print "{$a->operation()}\n" ;

$a->delegate = new Delegate() ;
print "{$a->operation()}\n" ;
