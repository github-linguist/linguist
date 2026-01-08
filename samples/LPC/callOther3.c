test() {    
    /** @type {"object.c"*} */
    object *o = ({});

    // call other on an array is legal in fluffos
    o->query_number();
}
