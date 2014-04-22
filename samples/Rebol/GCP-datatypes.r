Rebol [Title: "Google Code Prettify Datatypes"]

re: func [s /i] [rejoin compose ["/^^" (s) "/" either i ["i"][""]]]   ; little helper for standard grammar regex used

date-re: "\d{1,2}[\-\/](\d{1,2}|\w{3,9})[\-\/]\d{2,4}"  ; naive date! regex

string-re: {\"(?:[^^\"\\]|\\[\s\S])*(?:\"|$)}
brace-re:  "\{(?:[^^\}\^^]|\^^[\s\S])*(?:\}|$)"   ; TODO - could build this from string-re
block-re:  "\[(?:[^^\]\\]|\\[\s\S])*(?:\]|$)"
tag-re:    "\<(?:[^^\>\\]|\\[\s\S])*(?:\>|$)"   ; TODO - could build this from string-re
number-re: "(?:[.,]\d+|\d+['\d]*(?:[.,]\d*)?)(?:e[-+]?\d+)?"
word-re: "[A-Za-z=\-?!_*+.`~&][A-Za-z0-9=\-!?_*+.`~&]*"

|: "|"

types: compose/deep [
    ;  comments
    comment! [
        PR_LITERAL
    ;      comment_shebang -- Script tag (shebang!)
        (re/i "#![^^\r\n]+")
    ;      comment_line -- A line comment that starts with ;
        (re ";[^^\r\n]*")
    ;      comment_multiline_string -- Multi-line comment
        (re ["comment\s*" brace-re])
    ;      comment_multiline_block
        (re ["comment\s*" block-re])
    ]
    ;  type_literal
    
    ;  logic
    logic! [
        PR_LITERAL
        (re "#\[(?:true|false|yes|no|on|off)\]")
    ]
    
    ;  none
    none! [
        PR_LITERAL
        (re "#\[none\]")
    ]
    
    ;  strings
    ;      character
    char! [
        PR_LITERAL
        (re/i "#^"(?:[^^^^^"]|\^^(?:[\^^^"\/\-A-Z]|\((?:[0-9A-F]{2,4}|tab|newline)\)))^"")
    ]
    
    string! [
        PR_LITERAL
    ;      string_quoted
        (re "^"(?:[^^^"\\]|\\[\s\S])*(?:^"|$)")
    ;      string_multiline -- Multi-line string {braces} - allowed within:  { ^{ ^}
        (re brace-re)
    ]
    
    ;      string_tag_comment
    comment! [
        PR_LITERAL
        (re "<!--(?:[^^-]|-(?!->))+-->")
    ]
    
    ;      string_tag
    tag! [
        PR_LITERAL
        (re "<[^^^"<=>\x00\x09\x0A\x0D\x20\u005D\u007F][^^>\x00]*>")
    ]
    
    file! [
        PR_LITERAL
    ;      string_file
        (re "%(?:[a-zA-Z?!.*&|=_~0-9'+\-,:\/\\@]|%[0-9A-F]{2})+")
    ;      string_file_quoted
        (re "%^"(?:[^^^"])*^"")
    ]
    
    url! [
        PR_LITERAL
    ;      string_url
        (re "[a-zA-Z?!.*&|=_~][a-zA-Z?!.*&|=_~0-9'+-,]*:(?:[a-zA-Z?!.*&|=_~0-9'+\-,:\/@]|%[0-9A-F]{2})+")
    ]
    
    email! [
        PR_LITERAL
    ;      string_email
        (re "[\w\d\+\-\.]+\@[\w\d\+\-\.]+\b")
    ]
    
    binary! [
        PR_LITERAL
    ;      binary_base_two
        (re "2#\{(?:[01\r\n\t ])*\}")
    ;      binary_base_sixty_four
        (re "64#\{(?:[0-9+\/a-yA-Z=\r\n\t ])*\}")
    ;      binary_base_sixteen
        (re/i "(?:16)?#\{(?:[0-9a-f\r\n\t ])*\}")
    ]
    
    issue! [
        PR_LITERAL
    ;      string_issue
        (re "#[\w\d\-]+(?=[\s\n\t]|$)")
    ]
    
    ;  values
    date! [
        PR_LITERAL
    ;      value_date
        (re [date-re "\/\d{1,2}\:\d{1,2}\:\d{1,2}(\+|\-)\d{1,2}\:(00|30)\b"])
        (re [date-re "\/\d{1,2}\:\d{1,2}\:\d{1,2}\b"])
        (re [date-re "\b"])
        (re "\d{2,4}[\/\-](\d{1,2}|\w{3,9})[\/\-]\d{1,2}(?:\/\d{1,2}\:\d{1,2}(?:\:\d{1,2})?(?:[-+]\d{1,2}:[03]0)?)?")
    ]
    
    time! [
        PR_LITERAL
    ;      value_time
        (re "[-+]?\d{1,2}:\d{1,2}(?::\d{1,2}(?:\.\d+)?)?\b")
    ]
    
    tuple! [
        PR_LITERAL
    ;      value_tuple
        (re "\d+(?:\.\d+){2,9}")
    ]
    
    pair! [
        PR_LITERAL
    ;      value_pair
        (re/i ["[-+]?" number-re "x[-+]?" number-re])
    ]
    ;  [PR['PR_LITERAL'], /^\d(?:[\.\,\'\d]*)x\d(?:[\.\,\'\d]*)\b/]
    
    money! [
        PR_LITERAL
    ;      value_money
        (re ["[-+]?[A-Z]{0,3}\$" number-re])
        ;  [PR['PR_LITERAL'], /^\$\d[\d\.\,\']*\b/]
        ;  [PR['PR_LITERAL'], /^[\+\-\w]{1,4}\$\d[\d\.\,\']*\b/]
    ]
    
    ;      value_number
    number! [
        PR_LITERAL
        (re/i ["[-+]?" number-re "%?"])
        ;  percent! [PR_LITERAL (re "(\+|\-|\d)(?:[\.\,\'\d]*)\%\b")]
        ;  decimal! [PR_LITERAL (re "(\+|\-|\d)\d*(?:[\.\,]\d+)\b")]
        ;  integer! [PR_LITERAL (re "(\+|\-|\d)\d*\b")]
    ]
    
    ;  words
    datatype! [
        PR_LITERAL
    ;      word_datatype
        (re "(?:[A-Za-z\-]+)\!(?![A-Za-z0-9\-])")
    ]
    
    set-word! [
        PR_LITERAL
    ;      word_set
        (re [word-re "(?:\/" word-re "|\/\d+)*:"])
    ]
    
    ;  -- get-word!
    get-word! [
        PR_LITERAL
    ;      word_get
        (re [":" word-re])
    ]
    
    ;  -- lit-word!
    lit-word! [
        PR_LITERAL
    ;      word_lit
        (re ["'" word-re])
    ]
    
    refinement! [
        PR_LITERAL
    ;      word_refine
        (re reduce ["\/" replace copy find next word-re "[" "]*" "]+" "(?![A-Za-z0-9\-])"])
    ]

    op! [
        PR_LITERAL
    ;      word_native
        (re "(?:!=?=?|\*\*?|[+-]|\/\/?|<[=>]?|=[=?]?|>=?)(?![A-Za-z0-9\-])")
    ]
    
    function! [
        PR_LITERAL
        (re make-keywords-string)
        ; [REB['function!'], /\b(?:to\-relative\-file\/as\-local|or\~|pwd|abs|map|not|rm|at|do|dp|ds|dt|cd|in|ls|to|or|if)\s/]
    ]
    
    rebol! [
        PR_LITERAL
    ;      word_header
        (re/i "(?:rebol|red(?:\/system)?|world|topaz)$")
    ]
    
    logic! [
        PR_LITERAL
    ;      word_logic
        (re "(?:true|false|yes|no|on|off)$")
    ]
    
    ;      word_none
    none! [PR_LITERAL (re "none$")]
    
    ;      word
    word! [PR_LITERAL (re word-re)]

    ;  -- literal!
    ; literal! [PR_LITERAL (re ["#" block-re])]
]
