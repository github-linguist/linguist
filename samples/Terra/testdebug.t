if not terralib.traceback then return end
--this test require debug on, if it is not on, relaunch with it on
if 0 == terralib.isdebug then
  assert(0 == os.execute(terralib.terrahome.."/bin/terra -g testdebug.t"))
  return
end
C = terralib.includec("stdio.h")

terra foo(a : int, b : int)
  var c = a + b
  return c * 2
end

local ptr = terralib.cast(rawstring,foo:getdefinitions()[1]:getpointer())

terra findptr(a : &opaque)
  var si : terralib.SymbolInfo
  var li : terralib.LineInfo
  terralib.lookupsymbol(a,&si)
  C.printf("p = %p, addr = %p, sz = %d, nm = %.*s\n",a,si.addr,[int](si.size), si.namelength,si.name)
  terralib.lookupline(si.addr,a, &li)
  C.printf("line = %.*s:%d\n",li.namelength,li.name,[int](li.linenum))
  return li.linenum
end
assert(11 == findptr(ptr+6))
assert(10 == findptr(ptr+4))
local ra = terralib.intrinsic("llvm.returnaddress", int32 -> &opaque )
local fa = terralib.intrinsic("llvm.frameaddress", int32 -> &opaque )
terra testbt3()
  var frames : (&opaque)[128]
  terralib.traceback(nil)
  var N = terralib.backtrace(frames,128,ra(0),fa(1))
  for i = 0,N do
    C.printf("%p ",frames[i])
    var nm : rawstring
    var nmL : uint64 
    var si : terralib.SymbolInfo
    if terralib.lookupsymbol(frames[i],&si) then
      C.printf("frame %.*s\n", si.namelength, si.name)
    else
      C.printf("\n")
    end
  end
end
testbt3:setinlined(false)
terra fn2()
  testbt3()
  C.printf("fn2\n")
  return 1
end
terra what()
  fn2()
  C.printf("what\n")
end
fn2:setinlined(false)
what()
terralib.disas(terralib.traceback,5,5)