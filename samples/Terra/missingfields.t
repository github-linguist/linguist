test = terralib.includecstring([[
    typedef struct {} emptyanon;
    typedef struct foo foobar;
    void test(struct foo * f);
    struct foo { int x; };
    union ufoo;
    typedef union { int a; int b; } anonunion;
]])
terra main()
    var s : test.foo
    s.x = 1
end

main()