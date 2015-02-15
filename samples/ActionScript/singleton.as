package
{
    public class Singleton
    {

        private static var instance:Singleton;

        // ActionScript does not allow private or protected constructors.
        public function Singleton(enforcer:SingletonEnforcer) {

        }

        public static function getInstance():Singleton {
            if (instance == null) instance = new Singleton(new SingletonEnforcer());
            return instance;
        }
    }
}

internal class SingletonEnforcer {}
