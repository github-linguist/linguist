proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}
echo strcmp("abc", "def")
echo strcmp("hello", "hello")
