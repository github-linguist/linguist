USING: accessors assocs fry io kernel math.parser sequences
sorting ;
IN: top-rank

TUPLE: employee name id salary department ;

CONSTANT: employees {
        T{ employee f "Tyler Bennett" "E10297" 32000 "D101" }
        T{ employee f "John Rappl" "E21437" 47000 "D050" }
        T{ employee f "George Woltman" "E00127" 53500 "D101" }
        T{ employee f "Adam Smith" "E63535" 18000 "D202" }
        T{ employee f "Claire Buckman" "E39876" 27800 "D202" }
        T{ employee f "David McClellan" "E04242" 41500 "D101" }
        T{ employee f "Rich Holcomb" "E01234" 49500 "D202" }
        T{ employee f "Nathan Adams" "E41298" 21900 "D050" }
        T{ employee f "Richard Potter" "E43128" 15900 "D101" }
        T{ employee f "David Motsinger" "E27002" 19250 "D202" }
        T{ employee f "Tim Sampair" "E03033" 27000 "D101" }
        T{ employee f "Kim Arlich" "E10001" 57000 "D190" }
        T{ employee f "Timothy Grove" "E16398" 29900 "D190" }
    }

: group-by ( seq quot -- hash )
    H{ } clone [ '[ dup @ _ push-at ] each ] keep ; inline

: prepare-departments ( seq -- departments )
    [ department>> ] group-by
    [ [ salary>> ] inv-sort-with ] assoc-map ;

: first-n-each ( seq n quot -- )
    [ short head-slice ] dip each ; inline

: main ( -- )
    employees prepare-departments [
        [ "Department " write write ":" print ] dip
        3 [
            [ id>> write "  $" write ]
            [ salary>> number>string write "  " write ]
            [ name>> print ] tri
        ] first-n-each
        nl
    ] assoc-each ;
