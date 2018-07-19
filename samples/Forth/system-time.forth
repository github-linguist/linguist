[UNDEFINED] MS@ [IF]                                   \ Win32Forth (rolls over daily)
 [DEFINED] ?MS [IF] ( -- ms )
   : ms@ ?MS ;                                         \ iForth
 [ELSE] [DEFINED] cputime [IF] ( -- Dusec )
   : ms@ cputime d+ 1000 um/mod nip ;                  \ gforth: Anton Ertl
 [ELSE] [DEFINED] timer@ [IF] ( -- Dusec )
   : ms@ timer@ >us 1000 um/mod nip ;                  \ bigForth
 [ELSE] [DEFINED] gettimeofday [IF] ( -- usec sec )
   : ms@ gettimeofday 1000 MOD 1000 * SWAP 1000 / + ;  \ PFE
 [ELSE] [DEFINED] counter [IF]
   : ms@ counter ;                                     \ SwiftForth
 [ELSE] [DEFINED] GetTickCount [IF]
   : ms@ GetTickCount ;                                \ VFX Forth
 [ELSE] [DEFINED] MICROSECS [IF]
   : ms@  microsecs 1000 UM/MOD nip ;                  \  MacForth
[THEN] [THEN] [THEN] [THEN] [THEN] [THEN] [THEN]

MS@ .   \ print millisecond counter
