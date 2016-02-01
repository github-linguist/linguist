local ffi = require("ffi")
if ffi.os == "Windows" then
  return
end

C = terralib.includecstring [[
#include <pthread.h>
#include <stdio.h>
]]

acc = global(int[4])

terra forkedFn(args : &opaque) : &opaque
  var threadid = @[&int](args)
  C.printf("threadid %d\n",threadid)
  acc[threadid] = threadid
  return nil
end

terra foo()
  var thread0 : C.pthread_t
  var thread1 : C.pthread_t
  var thread2 : C.pthread_t
  var thread3 : C.pthread_t

  acc[0]=-42
  acc[1]=-42
  acc[2]=-42
  acc[3]=-42

  var args = arrayof(int,0,1,2,3)

  C.pthread_create(&thread0,nil,forkedFn,&args[0])
  C.pthread_create(&thread1,nil,forkedFn,&args[1])
  C.pthread_create(&thread2,nil,forkedFn,&args[2])
  C.pthread_create(&thread3,nil,forkedFn,&args[3])

  C.pthread_join(thread0,nil)
  C.pthread_join(thread1,nil)
  C.pthread_join(thread2,nil)
  C.pthread_join(thread3,nil)

  return acc[0]+acc[1]+acc[2]+acc[3]
end

local test = require("test")
test.eq(foo(),0+1+2+3)