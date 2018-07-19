require "md5"

--printing a sum:
print(md5.sumhexa"The quick brown fox jumps over the lazy dog")

--running the test suite:

local function test(msg,sum) assert(md5.sumhexa(msg)==sum) end

test("","d41d8cd98f00b204e9800998ecf8427e")
test("a","0cc175b9c0f1b6a831c399e269772661")
test("abc","900150983cd24fb0d6963f7d28e17f72")
test("message digest","f96b697d7cb7938d525a2f31aaf161d0")
test("abcdefghijklmnopqrstuvwxyz","c3fcd3d76192e4007dfb496cca67e13b")
test("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789","d174ab98d277d9f5a5611c2c9f419d9f")
test("12345678901234567890123456789012345678901234567890123456789012345678901234567890","57edf4a22be3c955ac49da2e2107b67a")
