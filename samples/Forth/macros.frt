\ Simplifies compiling words.

: [[     ; immediate
: '<>    >in @ ' swap >in ! <> ;
: (]])   begin dup '<> while postpone postpone repeat drop ;
: ]]     ['] [[ (]]) ; immediate

( Usage:   : foo ]] dup * [[ ; immediate   : bar 42 foo . ; )
