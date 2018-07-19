C:\Apps\groovy>groovysh
Groovy Shell (1.6.2, JVM: 1.6.0_13)
Type 'help' or '\h' for help.
---------------------------------------------------------------------------------------------------
groovy:000> f = { a, b, sep -> a + sep + sep + b }
===> groovysh_evaluate$_run_closure1@5e8d7d
groovy:000> println f('Rosetta','Code',':')
Rosetta::Code
===> null
groovy:000> exit

C:\Apps\groovy>
