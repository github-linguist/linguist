import java.io.*;

// classes must implement java.io.Serializable in order to be serializable
class Entity implements Serializable {
    // it is recommended to hard-code serialVersionUID so changes to class
    // will not invalidate previously serialized objects
    static final long serialVersionUID = 3504465751164822571L;
    String name = "Entity";
    public String toString() { return name; }
}

class Person extends Entity implements Serializable {
    static final long serialVersionUID = -9170445713373959735L;
    Person() { name = "Cletus"; }
}

public class SerializationTest {
    public static void main(String[] args) {
        Person instance1 = new Person();
        System.out.println(instance1);

        Entity instance2 = new Entity();
        System.out.println(instance2);

        // Serialize
        try {
            ObjectOutput out = new ObjectOutputStream(new FileOutputStream("objects.dat")); // open ObjectOutputStream

            out.writeObject(instance1); // serialize "instance1" and "instance2" to "out"
            out.writeObject(instance2);
            out.close();
            System.out.println("Serialized...");
        } catch (IOException e) {
            System.err.println("Something screwed up while serializing");
            e.printStackTrace();
            System.exit(1);
        }

        // Deserialize
        try {
            ObjectInput in = new ObjectInputStream(new FileInputStream("objects.dat")); // open ObjectInputStream

            Object readObject1 = in.readObject(); // read two objects from "in"
            Object readObject2 = in.readObject(); // you may want to cast them to the appropriate types
            in.close();
            System.out.println("Deserialized...");

            System.out.println(readObject1);
            System.out.println(readObject2);
        } catch (IOException e) {
            System.err.println("Something screwed up while deserializing");
            e.printStackTrace();
            System.exit(1);
        } catch (ClassNotFoundException e) {
            System.err.println("Unknown class for deserialized object");
            e.printStackTrace();
            System.exit(1);
        }
    }
}
