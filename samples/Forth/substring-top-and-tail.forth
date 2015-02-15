: hello ( -- c-addr u )
  s" Hello" ;

hello 1 /string type     \ => ello

hello 1- type            \ => hell

hello 1 /string 1- type  \ => ell
