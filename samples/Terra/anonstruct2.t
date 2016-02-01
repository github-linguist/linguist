
struct A { a : &A }

terra foo()
    var a : A    
end
foo()