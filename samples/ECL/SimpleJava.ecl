/*  The HPCC Systems platform comes bundled with a Java example class.
    Execute the following example in your favorite ECL IDE (or ECL Watch Playground) 
    If you are running this example without the Java plugin installed, you will get syntax errors
*/

IMPORT java;

INTEGER add1(integer val) := IMPORT(java, 'JavaCat.add1:(I)I');

add1(10);
