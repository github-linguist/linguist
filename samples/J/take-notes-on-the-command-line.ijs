require 'files strings'

notes=: monad define
 if. #y do.
   now=. LF ,~ 6!:0 'hh:mm:ss DD/MM/YYYY'
   'notes.txt' fappend~ now, LF ,~ TAB, ' ' joinstring y
 elseif. -. _1 -: txt=. fread 'notes.txt' do.
   smoutput txt
 end.
)

notes 2}.ARGV
exit 0
