: example ( -- )
readln readln [ string>number ] bi@
[ > [ "A > B" print ] when ]
[ < [ "A < B" print ] when ]
[ = [ "A = B" print ] when ] 2tri ;
