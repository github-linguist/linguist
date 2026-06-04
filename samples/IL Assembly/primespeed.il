.assembly extern System.Runtime
{
  .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A )
  .ver 8:0:0:0
}
.assembly extern System.Console
{
  .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A )
  .ver 8:0:0:0
}

.assembly HelloWorld {
  .custom instance void [System.Runtime]System.Runtime.CompilerServices.CompilationRelaxationsAttribute::.ctor(int32) = ( 01 00 08 00 00 00 00 00 )
  .custom instance void [System.Runtime]System.Runtime.Versioning.TargetFrameworkAttribute::.ctor(string) = ( 01 00 18 2E 4E 45 54 43 6F 72 65 41 70 70 2C 56   // ....NETCoreApp,V
                                                                                                              65 72 73 69 6F 6E 3D 76 38 2E 30 01 00 54 0E 14   // ersion=v8.0..T..
                                                                                                              46 72 61 6D 65 77 6F 72 6B 44 69 73 70 6C 61 79   // FrameworkDisplay
                                                                                                              4E 61 6D 65 08 2E 4E 45 54 20 38 2E 30 )          // Name..NET 8.0
  .hash algorithm 0x00008004
  .ver 1:0:0:0

}
.module HelloWorld.dll
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.corflags 0x00000001

.method public static void Main() cil managed
{
    .entrypoint
    .maxstack 1
    // Debugging information line must match the location of the source file
    .line 1,1 : 1,19 '/workspace/ILSamples/helloworld.txt'
    nop
    ldstr "Hello World"
    .line 2,2 : 1,6 ''
    nop
    call void [System.Console]System.Console::WriteLine(string)
    .line 3,3 : 0,1 ''
    nop
    ret
}