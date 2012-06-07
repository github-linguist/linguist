package example

enhancement Hello : String {

  function toPerson() : Person {
     var vals = this.split(",")
     return new Person( vals[0], vals[1] as int, Relationship.valueOf( vals[2] ) ) )
  }
}