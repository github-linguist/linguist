\d .log

h:-2              / handle to print log
lvl:2             / log level
unit:"BKMGTP"     / memory unit character
mult:5 (1024*)\ 1 / memory multiplier

/ build memory string
mem:{@[string"i"$(3#x)%mult m;2;,;unit m:mult bin x 2]}

/ build log header
hdr:{string[(.z.D;.z.T)],mem system "w"}

/ build log message
msg:{if[x<=lvl;h " " sv hdr[],(y;$[10h=type z;z;-3!z])]}

/ user level functions to log messages
err:msg[0;"[E]"]
wrn:msg[1;"[W]"]
inf:msg[2;"[I]"]
dbg:msg[3;"[D]"]
trc:msg[4;"[T]"]
