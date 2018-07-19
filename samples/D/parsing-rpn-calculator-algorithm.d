import std.stdio, std.string, std.conv, std.typetuple;

void main() {
    auto input = "3 4 2 * 1 5 - 2 3 ^ ^ / +";
    writeln("For postfix expression: ", input);
    writeln("\nToken            Action            Stack");
    real[] stack;
    foreach (tok; input.split()) {
        auto action = "Apply op to top of stack";
        switch (tok) {
            foreach (o; TypeTuple!("+", "-", "*", "/", "^")) {
                case o:
                    mixin("stack[$ - 2]" ~
                          (o == "^" ? "^^" : o) ~ "=stack[$ - 1];");
                    stack.length--;
                    break;
            }
            break;
            default:
                action = "Push num onto top of stack";
                stack ~= to!real(tok);
        }
        writefln("%3s    %-26s  %s", tok, action, stack);
    }
    writeln("\nThe final value is ", stack[0]);
}
