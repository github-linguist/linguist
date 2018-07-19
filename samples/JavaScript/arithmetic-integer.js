var a = parseInt(get_input("Enter an integer"), 10);
var b = parseInt(get_input("Enter an integer"), 10);

WScript.Echo("a = " + a);
WScript.Echo("b = " + b);
WScript.Echo("sum: a + b = "        + (a + b));
WScript.Echo("difference: a - b = " + (a - b));
WScript.Echo("product: a * b = "    + (a * b));
WScript.Echo("quotient: a / b = "   + (a / b | 0)); // "| 0" casts it to an integer
WScript.Echo("remainder: a % b = "  + (a % b));

function get_input(prompt) {
    output(prompt);
    try {
        return WScript.StdIn.readLine();
    } catch(e) {
        return readline();
    }
}
function output(prompt) {
    try {
        WScript.Echo(prompt);
    } catch(e) {
        print(prompt);
    }
}
