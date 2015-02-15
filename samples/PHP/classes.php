class MyClass {
    public static $classVar;
    public $instanceVar; // can also initialize it here
    function __construct() {
        $this->instanceVar = 0;
    }
    function someMethod() {
        $this->instanceVar = 1;
        self::$classVar = 3;
    }
}
$myObj = new MyClass();
