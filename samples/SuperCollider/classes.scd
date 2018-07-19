MyClass {
    classvar someVar, <another, <>thirdVar;    // Class variables.
    var <>something, <>somethingElse;           // Instance variables.
                   // Note: variables are private by default. In the above, "<" enables getting, ">" enables setting

    *new {
        ^super.new.init         // constructor is a class method. typically calls some instance method to set up, here "init"
    }

    init {
          something = thirdVar.squared;
          somethingElse = this.class.name;
    }

    *aClassMethod {
         ^  someVar + thirdVar         // The "^" means to return the result. If not specified, then the object itself will be returned ("^this")
    }

    anInstanceMethod {
        something = something + 1;
    }
}
