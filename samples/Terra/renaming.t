
C = terralib.includec("stdio.h")
terra main()
    C.printf("what\n")
end
main:setinlined(false)

terra realmain()
    main()
end

terra foo()
end
and terra foo(a : int)
end

foo:compile()

terralib.saveobj("renamed",{ main = realmain })

terralib.dumpmodule()