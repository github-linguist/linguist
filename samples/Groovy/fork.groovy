println "BEFORE PROCESS"
Process p = Runtime.runtime.exec('''
C:/cygwin/bin/sh -c "
/usr/bin/date +'BEFORE LOOP: %T';
for i in 1 2 3 4 ; do
    /usr/bin/sleep 1;
    /usr/bin/echo \$i;
done;
/usr/bin/date +'AFTER LOOP: %T'"
''')
p.consumeProcessOutput(System.out, System.err)
(0..<8).each {
    Thread.sleep(500)
    print '.'
}
p.waitFor()
println "AFTER PROCESS"
