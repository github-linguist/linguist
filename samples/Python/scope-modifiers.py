>>> x="From global scope"
>>> def outerfunc():
    x = "From scope at outerfunc"

    def scoped_local():
        x = "scope local"
        return "scoped_local scope gives x = " + x
    print(scoped_local())

    def scoped_nonlocal():
        nonlocal x
        return "scoped_nonlocal scope gives x = " + x
    print(scoped_nonlocal())

    def scoped_global():
        global x
        return "scoped_global scope gives x = " + x
    print(scoped_global())

    def scoped_notdefinedlocally():
        return "scoped_notdefinedlocally scope gives x = " + x
    print(scoped_notdefinedlocally())


>>> outerfunc()
scoped_local scope gives x = scope local
scoped_nonlocal scope gives x = From scope at outerfunc
scoped_global scope gives x = From global scope
scoped_notdefinedlocally scope gives x = From global scope
>>>
