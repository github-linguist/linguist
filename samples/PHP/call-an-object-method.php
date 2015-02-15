// Static method
MyClass::method($someParameter);
// In PHP 5.3+, static method can be called on a string of the class name
$foo = 'MyClass';
$foo::method($someParameter);


// Instance method
$myInstance->method($someParameter);
