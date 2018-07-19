class Foo
    @staticMethod: -> 'Bar'

    instanceMethod: -> 'Baz'

foo = new Foo

foo.instanceMethod() #=> 'Baz'
Foo.staticMethod() #=> 'Bar'
