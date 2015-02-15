val a = Console.readInt
val b = Console.readInt

val sum = a + b;//integer addition is discouraged in print statements due to confusion with String concatenation
println("a + b = " + sum);
println("a - b = " + (a - b));
println("a * b = " + (a * b));
println("quotient of a / b = " + (a / b)); // truncates towards 0
println("remainder of a / b = " + (a % b)); // same sign as first operand
