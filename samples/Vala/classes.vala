public class MyClass : Object {
    // Instance variable
    public int variable;

    // Method
    public void some_method() {
        variable = 24;
    }

    // Constructor
    public MyClass() {
        variable = 42;
    }
}
void main() {
    // Class instance
    MyClass instance = new MyClass();
    print("%d\n", instance.variable);
    instance.some_method();
    print("%d\n", instance.variable);
    instance.variable = 84;
    print("%d\n", instance.variable);
}
