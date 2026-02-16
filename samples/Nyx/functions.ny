# Nyx Functions Example
# Demonstrates functions, classes, and control flow

# Basic function
fn add(a, b) {
    return a + b;
}

# Function with default parameters
fn greet(name, greeting = "Hello") {
    return greeting + ", " + name + "!";
}

# Lambda function
let double = fn(x) { x * 2 };

# Class definition
class Person {
    fn init(self, name, age) {
        self.name = name;
        self.age = age;
    }
    
    fn introduce(self) {
        return "I'm " + self.name + ", " + str(self.age) + " years old";
    }
}

# Create instances
let john = new Person("John", 30);
print(john.introduce());

# Control flow
let x = 10;
if x > 5 {
    print("x is greater than 5");
} else {
    print("x is 5 or less");
}

# For loop
for i in range(5) {
    print("Count: " + str(i));
}

# Array operations
let numbers = [1, 2, 3, 4, 5];
let doubled = numbers.map(fn(n) { n * 2 });
print(doubled);
