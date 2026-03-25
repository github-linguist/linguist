// Exemple réel: Définition de structure et implémentation
struct Person {
    name: string,
    age: int,
    email: string?
}

impl Person {
    fn greet(self) {
        println("Hello, " + self.name)
    }
    
    fn is_adult(self) -> bool {
        ret self.age >= 18
    }
}

fn main() {
    lt alice = new Person {
        name: "Alice",
        age: 30,
        email: "alice@example.com"
    }
    
    alice.greet()
    println("Is adult: " + alice.is_adult())
    
    ret 0
}
