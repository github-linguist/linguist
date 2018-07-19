include ffl/est.fs
include ffl/xos.fs

\ Input lists
0 value names
here ," Emily"
here ," Tam O'Shanter"
here ," April"
here to names
, , ,

0 value remarks
here ,"  Short & shrift"
here ,\" Burns: \"When chapman billies leave the street ...\""
here ,"  Bubbly: I'm > Tam and <= Emily"
here to remarks
, , ,

: s++ ( c-addr1 -- c-addr2 c-addr3 u3 )
  dup cell+ swap @ count
;

\ Create xml writer
tos-create xml

: create-xml ( c-addr1 c-addr2 -- )
  0 s" CharacterRemarks" xml xos-write-start-tag
  3 0 DO
    swap s++ s" name" 2swap 1
    s" Character" xml xos-write-start-tag
    swap s++      xml xos-write-text
    s" Character" xml xos-write-end-tag
  LOOP
  drop drop
  s" CharacterRemarks" xml xos-write-end-tag
;

names remarks create-xml

\ Output xml string
xml str-get type cr
