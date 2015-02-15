: history create here cell+ , 0 , -1 , ;
: h@ @ @ ;
: h! swap here >r , dup @ , r> swap ! ;
: .history @ begin dup cell+ @ -1 <> while dup ? cell+ @ repeat drop ;
: h-- dup @ cell+ @ dup -1 = if abort" End of history" then swap ! ;
