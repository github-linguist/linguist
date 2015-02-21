@program cmd-say.muf
1 1000 d
i
( cmd-say.muf by Natasha@HLM

  Copyright 2002-2004 Natasha Snunkmeox. Copyright 2002-2004 Here Lie Monsters.
  "@view $box/mit" for license information.
)
$author Natasha Snunkmeox <natmeox@neologasm.org>
$note Say for Fuzzball 6.
$version 1.0

$include $lib/ignore
$include $lib/strings
$include $lib/match

$def str_program "saypose"
$def prop_third "_prefs/say/third"
$def prop_quotes "_say/def/quotes"
$def prop_overb "_say/def/osay"
$def prop_verb "_say/def/say"
$def prop_split "_prefs/say/split"
$def prop_color "_prefs/say/color"
$def prop_meow "_prefs/say/meow"

lvar randomWord

lvar verb
lvar overb
lvar lquo
lvar rquo
lvar splitsay

: rtn-getThirdVerb[ var:overb -- ]
    ( Get the third-person verb. )
    me @ prop_overb getpropstr dup if  ( str strOverb )
        strip dup dup "," instr not and if "," strcat then
    else pop "says," then  ( str strOverb )
    me @ "%D %s" fmtstring overb @ !  ( str )
;

: rtn-getFirstVerb[ var:verb var:overb -- ]
    me @ prop_third getpropstr .yes? not if  ( str )
        ( Get the first-person verb. )
        me @ prop_verb getpropstr dup if  ( str strVerb )
            strip dup dup "," instr not and if "," strcat then
        else pop "say," then  ( str strVerb )
        splitsay @ if "you %s" else "You %s" then fmtstring  ( str strVerb )
    else overb @ @ then verb @ !  ( str )
;

: rtn-getQuotes[ var:lquo var:rquo -- ]
    me @ prop_quotes getpropstr dup "%m" instr if  ( strQuotes )
        "%m" split  ( strLquo strRquo )
    else pop "\"" dup then  ( strLquo strRquo )
    rquo @ ! lquo @ !  (  )
;

: do-say  ( str -- )
    "" randomWord !

    var who
    var exclude

    ( Ignoring? Get 'em outta here. )
    loc @ contents_array  ( str arrHere )
    dup me @ str_program array_get_ignorers  ( str arrHere arrIgnorers )
    dup exclude !
    swap array_diff who !


    ( Anyone #meowing this player? Go ahead and notify before special formatting. )
    who @ prop_meow me @ owner "*{%d}*" fmtstring array_filter_prop  ( str arrMeow )
    dup if  ( str arrMeow )
        dup who @ array_diff who !  ( str arrMeow )
        dup exclude @ array_union exclude !  ( str arrMeow )

        over ansi_strip  ( str arrMeow str )
        "\\b[A-Z0-9_]+\\b" "MEOW" REG_ALL regsub  ( str arrMeow str' )
        "\\b[A-Z0-9_][A-Za-z0-9_]*[a-z][A-Za-z0-9_]*\\b" "Meow" REG_ALL regsub  ( str arrMeow str' )
        "\\b[a-z_][A-Za-z0-9_]*\\b" "meow" REG_ALL regsub  ( str arrMeow str' )
        me @ "%D meows, \"%s\"" fmtstring  ( str arrMeow str" )
        1 array_make swap array_notify  ( str )
    else pop then  ( str )


    var msg

    dup ",," instr  ( str boolCommas )
    me @ prop_split getpropstr .no? not  ( str boolCommas boolSplitOK )
    and if  ( str )
        ",," split  ( str- -str )

        ( User-supplied verb? )
        dup ",," instr if
            ",," split  ( str- strVerb -str )
            swap dup if  ( str- -str strVerb )
                strip  ( str- -str strVerb )
                dup me @ name instr over tolower "%n" instr or if  ( str- -str strVerb )
                    "%n" "%N" subst me @ name "%n" subst  ( str- -str strVerb )
                else
                    me @ swap "%s %D," fmtstring  ( str- -str -str- )
                then  ( str- -str -str- )
                dup "*[-!.,:;]" smatch not if "," strcat then  ( str- -str -str- )
                dup verb ! overb !  ( str- -str )
            else pop then  ( str- -str )
        then  ( str- -str )

        2 array_make  ( arrMsg )
        1
    else 0 then splitsay ! msg !


    verb @ string? not if
        overb rtn-getThirdVerb
        verb overb rtn-getFirstVerb
    then
    lquo rquo rtn-getQuotes  ( str )


    ( Say. )
    msg @ string? if
        rquo @ msg @ lquo @  ( strRquo strMsg strLquo )
        "%s %s%s%s"  ( strRquo strMsg strLquo strFormat )

        4 dupn
        verb  @ swap fmtstring .tell  ( strRquo strMsg strLquo strFormat )
        overb @ swap fmtstring  ( strOsay )
    else
        rquo @ msg @ array_vals pop  ( strRquo strMsg strMsg2 )
        swap dup "*[-!.,:;]" smatch not if "," strcat then swap  ( strRquo strMsg strMsg2 )
        ( Only handle strMsg if there's no strMsg2. )
        dup if  ( strRquo strMsg strMsg2 )
            swap  ( strRquo strMsg2 strMsg )
            lquo @ swap rquo @ swap lquo @  ( strRquo strMsg2 strLquo strRquo strMsg' strLquo )
            "%s%s%s %s %s%s%s"  ( strRquo strMsg2 strLquo strRquo strMsg' strLquo strFormat )

            7
        else  ( strRquo strMsg strMsg2 )
            pop lquo @  ( strRquo strMsg' strLquo )
            "%s%s%s %s"  ( strRquo strMsg' strLquo strFormat )

            verb @ ",$" "." 0 regsub verb !
            overb @ ",$" "." 0 regsub overb !

            4
        then  ( ... strRquo strMsg strLquo strFormat intDepth )

        dupn
        verb  @ -5 rotate fmtstring .tell  ( ... strRquo strMsg strLquo strFormat )
        overb @ -5 rotate fmtstring  ( strOsay )
    then  ( strOsay )


    ( Is there color to avoid? )
    dup "\[[" instr if
        who @ prop_color "{n*|0}" array_filter_prop  ( strOsay arrGreyed )
        dup if  ( strOsay arrGreyed )
            over ansi_strip 1 array_make  ( strOsay arrGreyed arrMsg )
            over array_notify  ( strOsay arrGreyed )

            exclude @ array_union exclude !  ( strOsay )
        else pop then  ( strOsay )
    then  ( strOsay )

    loc @  ( strOsay db )
    exclude @ array_vals  ( strOsay db dbExcludeN..dbExclude1 intN )
    me @ swap ++  ( strOsay db dbGreyedN..dbGreyed1' intN' )
    dup 3 + rotate  ( db dbGreyedN..dbGreyed1 intN strOsay )
    notify_exclude  (  )
;

: do-help pop pop .showhelp ;
: do-ignore pop str_program cmd-ignore-add ;
: do-unignore pop str_program cmd-ignore-del ;

: do-third  ( strY strZ -- )
    pop pop  (  )
    me @ prop_third "yes" setprop
    me @ "You will see your own says in the third person (\"%D says\")." fmtstring .tellgood
;
: do-unthird  ( strY strZ -- )
    pop pop  (  )
    me @ prop_third remove_prop
    "You will see your own says in the second person (\"You say\")." .tellgood
;

: do-grey  ( strY strZ -- )
    pop pop  (  )
    me @ prop_color "no" setprop
    me @ "You will not see color in any says. Note you will see color in your own says." fmtstring .tellgood
;
: do-ungrey  ( strY strZ -- )
    pop pop  (  )
    me @ prop_color remove_prop
    "You will see color in says." .tellgood
;

: do-meow  ( strY strZ -- )
    pop  ( strY )
    dup if
        .noisy_pmatch dup ok? not if pop exit then  ( db )
        me @ prop_meow 3 pick reflist_find if  ( db )
            "%D is already in your #meow list." fmtstring .tellbad exit  (  )
        then  ( db )
        me @ prop_meow 3 pick reflist_add  ( db )
        "%D added." fmtstring .tellgood
    else
        me @ prop_meow array_get_reflist  ( arr )
        "" swap foreach swap pop "%D %s" fmtstring repeat
        "Your meowlist: " swap strcat .tellgood
    then
;
: do-unmeow  ( strY strZ -- )
    pop  ( strY )
    .noisy_pmatch dup ok? not if pop exit then  ( db )
    me @ prop_meow 3 pick reflist_find not if  ( db )
        "%D is not in your #meow list." fmtstring .tellbad exit  (  )
    then  ( db )
    me @ prop_meow 3 pick reflist_del  ( db )
    "%D removed." fmtstring .tellgood
;

$define dict_commands {
    "help"    'do-help
    "ignore"  'do-ignore
    "!ignore" 'do-unignore
    "meow"    'do-meow
    "!meow"   'do-unmeow
    "third"   'do-third
    "!third"  'do-unthird
    "grey"    'do-grey
    "gray"    'do-grey
    "!grey"   'do-ungrey
    "!gray"   'do-ungrey
}dict $enddef

: main  ( str -- )
    dup STRparse  ( str strX strY strZ )
    3 pick string? if 3 pick "#" stringpfx if  ( str strX strY strZ )
        pop pop pop  ( str )
        "#" split strcat  ( str' )
        do-say exit  (  )
    then then
    3 pick int? if pop pop pop do-say exit then
    4 rotate pop  ( strX strY strZ )

    rot dict_commands over array_getitem  ( strY strZ strX ? )
    dup address? if  ( strY strZ strX adr )
        swap pop  ( strY strZ adr )
        execute  (  )
    else pop  ( strY strZ strX )
        "I don't recognize the command '#%s'. Try 'say #help' for help, or using '##' to say something starting with '#'." fmtstring .tellbad  ( strY strZ )
        pop pop  (  )
    then  (  )
;
.
c
q

lsedit #257=_help
.del 1 $
say <message>
."<message>
say #[!]ignore <names>
say #[!]third
say #[!]grey
say #[!]meow <names>

Speaks <message> to the room. Use #ignore <name> to not see <name>'s says, poses, and spoofs; use #meow <name> to see <name>'s says with all the words replaced with "meow." Use #third to see your own says in the third person (that is, "Puck says" instead of the normal "You say"). Use #grey to turn off color in others' says and poses.

Say supports a "split" say if you put two consecutive commas in your message. For example, if CobaltBlue typed '"Hello,,how are you?' everyone would see '"Hello," says CobaltBlue, "how are you?"' You can also specify an "ad-hoc" verb by putting a message with your name or '%N' between pairs of commas: '"Hello,,%N welcomes Weiran,,how are you?' would display '"Hello," CobaltBlue welcomes Weiran, "how are you?"'
.format 10=78
.format 8=78
.end
