# Class method
MyClass->classMethod($someParameter);
# Equivalently using a class name
my $foo = 'MyClass';
$foo->classMethod($someParameter);


# Instance method
$myInstance->method($someParameter);

# Calling a method with no parameters
$myInstance->anotherMethod;

# Class and instance method calls are made behind the scenes by getting the function from
# the package and calling it on the class name or object reference explicitly
MyClass::classMethod('MyClass', $someParameter);
MyClass::method($myInstance, $someParameter);
