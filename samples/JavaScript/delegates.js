function Delegator() {
  this.delegate = null ;
  this.operation = function(){
    if(this.delegate && typeof(this.delegate.thing) == 'function')
      return this.delegate.thing() ;
    return 'default implementation' ;
  }
}

function Delegate() {
  this.thing = function(){
    return 'Delegate Implementation' ;
  }
}

function testDelegator(){
  var a = new Delegator() ;
  document.write(a.operation() + "\n") ;

  a.delegate = 'A delegate may be any object' ;
  document.write(a.operation() + "\n") ;

  a.delegate = new Delegate() ;
  document.write(a.operation() + "\n") ;
}
