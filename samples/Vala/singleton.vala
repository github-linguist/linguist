public class Singleton : Object {
    static Singleton? instance;

    // Private constructor
    Singleton() {

    }

    // Public constructor
    public static Singleton get_instance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}

void main() {
    Singleton a = Singleton.get_instance();
    Singleton b = Singleton.get_instance();
    if (a == b) {
        print("Equal.\n");
    }
}
