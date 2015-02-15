class Singleton {
  protected static $instance = null;
  public $test_var;
  private function __construct(){
    //Any constructor code
  }
  public static function getInstance(){
    if (is_null(self::$instance)){
      self::$instance = new self();
    }
    return self::$instance;
  }
}

$foo = Singleton::getInstance();
$foo->test_var = 'One';

$bar = Singleton::getInstance();
echo $bar->test_var; //Prints 'One'

$fail = new Singleton(); //Fatal error
