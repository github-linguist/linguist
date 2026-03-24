struct Person {
    name: string,
    age: int
}

impl Person {
    fn greet(self) {
        println("Hello, " + self.name)
    }
}
