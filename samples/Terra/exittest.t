
C = { exit = terralib.externfunction("exit", int -> {}) }


terra moo()
    C.exit(0)
end

moo()