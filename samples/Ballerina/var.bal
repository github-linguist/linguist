import ballerina.lang.system;

function divideBy10 (int d) (int, int) {
    return d / 10, d % 10;
}

function main (string[] args) {
    //Here the variable type is inferred type from the initial value. This is same as "int k = 5";
    var k = 5;
    system:println(10 + k);

    //Here the type of the 'strVar' is 'string'.
    var strVar = "Hello!";
    system:println(strVar);

    //Multiple assignment with 'var' allows you to define the variable then and there.
    //Variable type is inferred from the right-hand side.
    var q, r = divideBy10(6);
    system:println("06/10: " + "quotient=" + q + " " +
                   "remainder=" + r);

    //To ignore a particular return value in a multiple assignment statement, use '_'.
    var q1, _ = divideBy10(57);
    system:println("57/10: " + "quotient=" + q1);

    var _, r1 = divideBy10(9);
    system:println("09/10: " + "remainder=" + r1);
}
