package example

uses java.util.*

uses java.io.File

class Person extends Contact implements IEmailable {

  var _name : String
  var _age : Integer as Age 
  var _relationship : Relationship as readonly RelationshipOfPerson

  delegate _emailHelper represents IEmailable

  enum Relationship {
    FRIEND,
    FAMILY,
    BUSINESS_CONTACT
  }

  // Map of names to people
  static var ALL_PEOPLE = new HashMap<String, Person>()

  /* Constructs a new Person */
  construct( name : String, age : Integer, relationship : Relationship ) {
    _name = name
    _age = age
    _relationship = relationship
    _emailHelper = new EmailHelper( this )
  }

  property get Name():String{
    return _name
  }

  property set Name(name : String){
    _name = name
  }

  /* Implement IEmailable#getEmailName() */
  override function getEmailName():String{
    return Name
  }

  function incrementAge() {
    _age++
  }

  @Deprecated
  function printPersonInfo() {
    print( "Person { Name : ${Name}, Age : ${Age}, Relationship : ${RelationshipOfPerson} }" )
  }

  static function addPerson(p : Person){
    if(ALL_PEOPLE.containsKey(p?.Name)) {
      throw new IllegalArgumentException( "There is already someone named '${p.Name}'." )
    }
    ALL_PEOPLE[p.Name] = p
  }

  static function addAllPeople( contacts : List<Contact> ) {
    for( contact in contacts ) {
      if( contact typeis Person and not ALL_PEOPLE.containsKey( contact.Name )) {
        addPerson( contact )
      }
    }
  }

  static function getAllPeopleOlderThanNOrderedByName( age : int ) {
    var allPeople = ALL_PEOPLE.Values

    return allPeople.where( \ p -> p.Age > age ).orderBy( \ p -> p.Name )
  }

  static function loadPersonFromDB( id : Integer ) {
    using( var conn = DBConnectionManager.getConnection(),
      var stmt = conn.prepareStatement( "SELECT name, age, relationship FROM PEOPLE WHERE ID=?") ){

      stmt.setInt( 0, 0 )
      var result = stmt.executeQuery()
      if( result.next() ) {
         addPerson( new Person( result.getString( "name" ),
                    result.getInt( "age" ),
                    Relationship.valueOf( result.getString( "relationship" ) ) ) )

      }
    }
  }

  /* Loads in people from a CSV */
  static function loadFromFile( file : File ) {
    file.eachLine( \ line -> {
      if( line.HasContent ) {
        addPerson( line.toPerson() )
      }
    })
  }

  /* Save people to a CSV */
  static function saveToFile( file : File ) {
    using( var writer = new FileWriter( file ) ) {
      print( PersonCSVTemplate.renderToString( ALL_PEOPLE.Values ) )
      PersonCSVTemplate.render( writer, ALL_PEOPLE.Values )
    }
  }
}