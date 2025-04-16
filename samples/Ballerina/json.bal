import ballerina.lang.system;

function main (string[] args) {
    // JSON string value.
    json j1 = "Apple";
    system:println(j1);

    // JSON number value.
    json j2 = 5.36;
    system:println(j2);

    // JSON true value.
    json j3 = true;
    system:println(j3);

    // JSON false value.
    json j4 = false;
    system:println(j4);

    // JSON null value.
    json j5 = null;

    //JSON Objects.
    json j6 = {name:"apple", color:"red", price:j2};
    system:println(j6);

    //JSON Arrays. They are arrays of any JSON value.
    json j7 = [1, false, null, "foo",
               {first:"John", last:"Pala"}];
    system:println(j7);
}
