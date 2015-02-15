include ffl/est.fs
include ffl/str.fs
include ffl/xis.fs

\ Build input string
str-create xmlstr
: x+ xmlstr str-append-string ;

s\" <Students>\n" x+
s\" <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n" x+
s\" <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"  x+
s\" <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n" x+
s\" <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"   x+
s\" <Pet Type=\"dog\" Name=\"Rover\" />\n" x+
s\" </Student>\n" x+
s\" <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n" x+
s\" </Students>\n" x+

\ Setup xml parser
xis-create xmlparser
xmlstr str-get xmlparser xis-set-string

\ Parse the xml
: xmlparse
  BEGIN
    xmlparser xis-read  dup xis.error <> over xis.done <> AND
  WHILE
    dup xis.start-tag = over xis.empty-element = OR IF
      drop
      s" Student" compare 0= IF
        0 ?DO
          2swap s" Name" compare 0= IF
            type cr
          ELSE
            2drop
          THEN
        LOOP
      ELSE
        xis+remove-attribute-parameters
      THEN
    ELSE
      xis+remove-read-parameters
    THEN
  REPEAT
  drop
;

xmlparse
