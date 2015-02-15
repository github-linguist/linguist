import std.stdio, std.random, std.math, std.algorithm, std.range,
       std.typetuple;

void main() {
    void op(char c)() {
        if (stack.length < 2)
            throw new Exception("Wrong expression.");
        stack[$ - 2] = mixin("stack[$ - 2]" ~ c ~ "stack[$ - 1]");
        stack.popBack();
    }

    const problem = iota(4).map!(_ => uniform(1, 10))().array();
    writeln("Make 24 with the digits: ", problem);

    double[] stack;
    int[] digits;
    foreach (const char c; readln())
        switch (c) {
            case ' ', '\t', '\n': break;
            case '1': .. case '9':
                stack ~= c - '0';
                digits ~= c - '0';
                break;
            foreach (o; TypeTuple!('+', '-', '*', '/')) {
                case o: op!o(); break;
            }
            break;
            default: throw new Exception("Wrong char: " ~ c);
        }

    if (!digits.sort().equal(problem.dup.sort()))
        throw new Exception("Not using the given digits.");
    if (stack.length != 1)
        throw new Exception("Wrong expression.");
    writeln("Result: ", stack[0]);
    writeln(abs(stack[0] - 24) < 0.001 ? "Good job!" : "Try again.");
}
