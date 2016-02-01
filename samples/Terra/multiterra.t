local ffi = require("ffi")

C = terralib.includecstring [[
#ifndef _WIN32
#include <pthread.h>
#endif
#include <stdio.h>
typedef struct lua_State lua_State;
lua_State * luaL_newstate();
int terra_init(lua_State * L);
int luaL_openlibs(lua_State * L);
int luaL_loadstring(lua_State * L, const char *);
int lua_close(lua_State * L);
void lua_call(lua_State * L, int,int);
int lua_pushnumber(lua_State * L, double);
int terra_loadstring(lua_State *L, const char *s);
double luaL_checknumber(lua_State * L,int);
]]

if ffi.os == "Windows" then
  -- fake it on windows
  C.pthread_t = int
  C.pthread_create = terra(t : &int, stuff : &opaque,fn : &opaque -> &opaque, data : &opaque)
    return fn(data)
  end
  C.pthread_join = terra(t : int, stuff : &opaque)
  end 
end


N = 4

local acc = global(int[N])

terra forkedFn(args : &opaque) : &opaque
  var threadid = @[&int](args)
  C.printf("threadid %d\n",threadid)
  
  var L = C.luaL_newstate();
  if L == nil then
    C.printf("can't initialize luajit\n")
  end
  
  C.luaL_openlibs(L)
  C.terra_init(L)

  C.terra_loadstring(L, [[ a = ...; C = terralib.includec("stdio.h"); terra foo () C.printf("new terra %d\n",a) return a end; return foo() ]])
  C.lua_pushnumber(L,threadid)
  C.lua_call(L,1,1)
  acc[threadid] = C.luaL_checknumber(L,-1)
  C.lua_close(L)

  return nil
end

terra foo()
  var thread : C.pthread_t[N]
  for i = 0,N do
    acc[i] = -1
  end

  var args : int[N]
  for i = 0,N do
    args[i] = i
    C.pthread_create(&thread[i],nil,forkedFn,&args[i])
  end

  for i = 0,N do
    C.pthread_join(thread[i],nil)
  end
  var sum = 0
  for i = 0,N do
    sum = sum + acc[i]
  end
  return sum
end

print(foo())
assert(foo() == N * (N - 1) / 2)