class Foo {
    name string
    age int @assert( {{ this > 10 }} )
}

class Bar {
    foo Foo @check(something, {{ this.name|regexMatch("Maya") }})
    @@dynamic
}