class T implements Cloneable {
    public String name() { return "T"; }
    public T copy() {
        try {
            return (T)super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }
    }
}

class S extends T {
    public String name() { return "S"; }
}

public class PolymorphicCopy {
    public static T copier(T x) { return x.copy(); }
    public static void main(String[] args) {
        T obj1 = new T();
        S obj2 = new S();
        System.out.println(copier(obj1).name()); // prints "T"
        System.out.println(copier(obj2).name()); // prints "S"
    }
}
