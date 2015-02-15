include lib/memcell.4th
include 4pp/lib/foos.4pp
                                       ( a1 -- a2)
:token fork dup allocated dup (~~alloc) swap >r swap over r> smove ;
                                       \ allocate an empty object
:: T()                                 \ super class T
   class
     method: print                     \ print message
     method: clone                     \ clone yourself
   end-class {
                                       \ implementing methods
     :method { ." class T" cr } ; defines print
     fork defines clone                ( -- addr)
   }
;

:: S()                                 \ class S
   extends T()                         \ derived from T
   end-extends {                       \ print message
     :method { ." class S" cr } ; defines print
   }                                   \ clone yourself
;

new T() t                              \ create a new object t
new S() s                              \ create a new object s

." before copy" cr
t => print                             \ use "print" methods
s => print

t => clone to tcopy                    \ cloning t, spawning tcopy
s => clone to scopy                    \ cloning s, spawning scopy

." after copy" cr
tcopy => print                         \ use "print" methods
scopy => print
