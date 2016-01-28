local S = require "std"

c = global(int,0)

struct A(S.Object) {
    a : int;
    b : int[5];
}
struct C {
    a : int
}
struct B(S.Object) {
    a : A;
    b : A[2];
    c : C[3];
    c2 : C;
    c3 : C[2];
}

terra A:__destruct()
    S.printf("A dtor!\n")
    c = c + 1
end
terra B:__destruct()
    c = c + 10
end
terra foo()
    var a = A.salloc()
end

foo()
assert(c:get() == 1)

terra bar()
    var b = B.salloc()
end
bar()
assert(c:get() == 14)

terra baz()
    var b = B.alloc()
    b:delete()
end
baz()
assert(c:get() == 27)