class Example
    def foo
        puts "this is foo"
    end
    def bar
        puts "this is bar"
    end
    def method_missing(name, *args, &block)
        puts "tried to handle unknown method %s" % name # name is a symbol
        unless args.empty?
            puts "it had arguments: %p" % [args]
        end
    end
end

example = Example.new

example.foo          # prints “this is foo”
example.bar          # prints “this is bar”
example.grill        # prints “tried to handle unknown method grill”
example.ding("dong") # prints “tried to handle unknown method ding”
                     # prints “it had arguments: ["dong"]”
