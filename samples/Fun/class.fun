
########################################
# class
########################################
#   class name ( parameters )
#     cmds
#   end class;
########################################

# define a class
class CA()
  # test for class constructor
  ? 'CA-constructor-starting..';

  # member variable
  var name = 'CA';

  # member function
  fun showName()
    ?. name;
  end fun;

  fun getName()
    return name;
  end fun;

  ?. 'DONE';
end class;

# create a class instance (object)
var a = CA();

# use the member variable of object
?. a.name;

# use the member function of object
a.showName();
?. a.getName();
