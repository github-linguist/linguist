function Fizzbuzz(n) {
    for (local i = 1; i <= n; i += 1) {
        if (i % 15 == 0)
            print ("FizzBuzz\n")
        else if (i % 5 == 0)
            print ("Buzz\n")
        else if (i % 3 == 0)
            print ("Fizz\n")
        else {
            print (i + "\n")
        }
    }
}
Fizzbuzz(100);
