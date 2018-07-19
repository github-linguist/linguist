#include <string>
#include <fstream>
#include <boost/serialization/string.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/base_object.hpp>
#include <iostream>

class Employee {
public :
   Employee( ) { }

   Employee ( const std::string &dep , const std::string &namen )
      : department( dep ) , name( namen ) {
	 my_id = count++ ;
      }

   std::string getName( ) const {
      return name ;
   }

   std::string getDepartment( ) const {
      return department ;
   }

   int getId( ) const {
      return my_id ;
   }

   void setDepartment( const std::string &dep ) {
      department.assign( dep ) ;
   }

   virtual void print( ) {
      std::cout << "Name: " << name << '\n' ;
      std::cout << "Id: " << my_id << '\n' ;
      std::cout << "Department: " << department << '\n' ;
   }

   virtual ~Employee( ) { }
   static int count ;
private :
   std::string name ;
   std::string department ;
   int my_id ;
   friend class boost::serialization::access ;

   template <class Archive>
      void serialize( Archive &ar, const unsigned int version ) {
	 ar & my_id ;
	 ar & name ;
	 ar & department ;
      }

} ;

class Worker : public Employee {
public :
   Worker( const std::string & dep, const std::string &namen ,
	 double hourlyPay ) : Employee( dep , namen ) , salary( hourlyPay) { }

   Worker( ) { }

   double getSalary( ) {
      return salary ;
   }

   void setSalary( double pay ) {
      if ( pay > 0 )
	 salary = pay ;
   }

   virtual void print( ) {
      Employee::print( ) ;
      std::cout << "wage per hour: " << salary << '\n' ;
   }
private :
   double salary ;
   friend class boost::serialization::access ;
   template <class Archive>
      void serialize ( Archive & ar, const unsigned int version ) {
	 ar & boost::serialization::base_object<Employee>( *this ) ;
	 ar & salary ;
      }
} ;

int Employee::count = 0 ;

int main( ) {
   std::ofstream storefile( "/home/ulrich/objects.dat"  ) ; //creating objects of base class
   const Employee emp1( "maintenance" , "Fritz Schmalstieg"  ) ;
   const Employee emp2( "maintenance" , "John Berry" ) ;
   const Employee emp3( "repair" , "Pawel Lichatschow" ) ;
   const Employee emp4( "IT" , "Marian Niculescu" ) ;
   const Worker worker1( "maintenance" , "Laurent Le Chef" , 20 ) ;//creating objects of derived class
   const Worker worker2 ( "IT" , "Srinivan Taraman" , 55.35 ) ;
   boost::archive::text_oarchive oar ( storefile ) ;//starting serialization into designated file
   oar << emp1 ;
   oar << emp2 ;
   oar << emp3 ;
   oar << emp4 ;
   oar << worker1 ;
   oar << worker2 ;
   storefile.close( ) ;
   std::cout << "Reading out the data again\n" ;
   Employee e1 , e2 , e3 , e4 ; //creating instances of base class objects for deserialization
   Worker w1, w2 ; // same for objects of derived class
   std::ifstream sourcefile( "/home/ulrich/objects.dat"  ) ;
   boost::archive::text_iarchive iar( sourcefile ) ;//starting deserialization
   iar >> e1 >> e2 >> e3 >> e4 ;
   iar >> w1 >> w2 ;
   sourcefile.close( ) ;
   std::cout << "And here are the data after deserialization!( abridged):\n" ;
   e1.print( ) ;
   e3.print( ) ;
   w2.print( ) ;
   return 0 ;
}
