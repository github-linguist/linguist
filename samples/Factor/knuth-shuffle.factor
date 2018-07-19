: randomize ( seq -- seq )
    dup length [ dup 1 > ]
    [ [ iota random ] [ 1 - ] bi [ pick exchange ] keep ]
    while drop ;
