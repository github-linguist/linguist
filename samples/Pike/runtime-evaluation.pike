program demo = compile_string(#"
    string name=\"demo\";
    string hello()
    {
       return(\"hello, i am \"+name);
    }");

demo()->hello();
Result: "hello, i am demo"
