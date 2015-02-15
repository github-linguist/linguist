namespace eval foo {
    # Define a procedure with two formal arguments; they are local variables
    proc bar {callerVarName argumentVar} {
        ### Associate some non-local variables with the procedure
        global globalVar;      # Variable in global namespace
        variable namespaceVar; # Variable in local (::foo) namespace
        # Access to variable in caller's context; may be local or global
        upvar 1 callerVarName callerVar

        ### Reading a variable uses the same syntax in all cases
        puts "caller's var has $callerVar"
        # But global and namespace vars can be accessed by using qualified names
        puts "global var has $globalVar which is $::globalVar"

        ### Writing a variable has no special syntax
        ### but [set] is by far the most common command for writing
        set namespaceVar $globalVar
        incr globalVar

        ### Destroying a variable is done like this
        unset argumentVar
    }
}
