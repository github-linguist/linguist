// Exemple réel: Module mathématique
packet math {
    fn add(a: int, b: int) -> int {
        ret a + b
    }
    
    fn subtract(a: int, b: int) -> int {
        ret a - b
    }
    
    fn multiply(a: int, b: int) -> int {
        ret a * b
    }
    
    fn divide(a: int, b: int) -> int {
        if b != 0 {
            ret a / b
        }
        ret 0
    }
    
    fn factorial(n: int) -> int {
        if n <= 1 {
            ret 1
        }
        ret n * factorial(n - 1)
    }
}
