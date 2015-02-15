>foo = Object.new  # => #<Object:0x10ae32000>
>id = foo.object_id  # => 2238812160
>"%x" % (id << 1)  # => "10ae32000"
