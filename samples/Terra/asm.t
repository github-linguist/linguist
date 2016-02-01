local a = ...
if not a then
    --force MCJIT
    os.execute("../terra -m asm.t true")
    return
end

C = terralib.includec("stdio.h")

struct Vendor {
    maxV : int;
    b : int;
    c : int;
    d : int;
}

terra foo()
    var r = terralib.asm(Vendor,"cpuid","={eax},={ebx},={ecx},={edx},{eax},~{dirflag},~{fpsr},~{flags}",false,0)
    r.c,r.d = r.d,r.c
    C.printf("%.12s\n", &r.b)
end
foo()


terra addone(a : int)
    return terralib.asm(int,"addl $$1,$1","=r,0",true,a)
end
assert(addone(3) == 4)