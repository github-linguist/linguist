load'task'

NB.  Execute a command and wait for it to complete
shell 'dir'

NB.  Execute a command but don't wait for it to complete
fork 'notepad'

NB.  Execute a command and capture its stdout
stdout   =:  shell 'dir'

NB.  Execute a command, provide it with stdin,
NB.  and capture its stdout
stdin    =:  'blahblahblah'
stdout   =:  stdin spawn 'grep blah'
