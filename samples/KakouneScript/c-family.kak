# Detection
hook global BufCreate .*\.(cc|cpp|cxx|C|hh|hpp|hxx|H)$ %{
    set-option buffer filetype cpp
}

hook global BufSetOption filetype=c\+\+ %{
    hook -once buffer NormalIdle '' "set-option buffer filetype cpp"
}

hook global BufCreate .*\.c$ %{
    set-option buffer filetype c
}

hook global BufCreate .*\.h$ %{
    try %{
        execute-keys -draft %{%s\b::\b|\btemplate\h*<lt>|\bclass\h+\w+|\b(typename|namespace)\b|\b(public|private|protected)\h*:<ret>}
        set-option buffer filetype cpp
    } catch %{
        set-option buffer filetype c
    }
}

hook global BufCreate .*\.m %{
    set-option buffer filetype objc
}

hook global WinSetOption filetype=(c|cpp|objc) %[
    require-module c-family

    evaluate-commands "set-option window static_words %%opt{%val{hook_param_capture_1}_static_words}"

    hook -group "%val{hook_param_capture_1}-trim-indent" window ModeChange pop:insert:.* c-family-trim-indent
    hook -group "%val{hook_param_capture_1}-insert" window InsertChar \n c-family-insert-on-newline
    hook -group "%val{hook_param_capture_1}-indent" window InsertChar \n c-family-indent-on-newline
    hook -group "%val{hook_param_capture_1}-indent" window InsertChar \{ c-family-indent-on-opening-curly-brace
    hook -group "%val{hook_param_capture_1}-indent" window InsertChar \} c-family-indent-on-closing-curly-brace
    hook -group "%val{hook_param_capture_1}-insert" window InsertChar \} c-family-insert-on-closing-curly-brace

    alias window alt "%val{hook_param_capture_1}-alternative-file"

    hook -once -always window WinSetOption filetype=.* "
        remove-hooks window %val{hook_param_capture_1}-.+
        unalias window alt %val{hook_param_capture_1}-alternative-file
    "
]

hook -group c-highlight global WinSetOption filetype=c %{
    add-highlighter window/c ref c
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/c }
}

hook -group cpp-highlight global WinSetOption filetype=cpp %{
    add-highlighter window/cpp ref cpp
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/cpp }
}

hook -group objc-highlight global WinSetOption filetype=objc %{
    add-highlighter window/objc ref objc
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/objc }
}


provide-module c-family %§

define-command -hidden c-family-trim-indent %{
    # remove the line if it's empty when leaving the insert mode
    try %{ execute-keys -draft <a-x> 1s^(\h+)$<ret> d }
}

define-command -hidden c-family-indent-on-newline %< evaluate-commands -draft -itersel %<
    execute-keys <semicolon>
    try %<
        # if previous line is part of a comment, do nothing
        execute-keys -draft <a-?>/\*<ret> <a-K>^\h*[^/*\h]<ret>
    > catch %<
        # else if previous line closed a paren (possibly followed by words and a comment),
        # copy indent of the opening paren line
        execute-keys -draft k<a-x> 1s(\))(\h+\w+)*\h*(\;\h*)?(?://[^\n]+)?\n\z<ret> m<a-semicolon>J <a-S> 1<a-&>
    > catch %<
        # else indent new lines with the same level as the previous one
        execute-keys -draft K <a-&>
    >
    # remove previous empty lines resulting from the automatic indent
    try %< execute-keys -draft k <a-x> <a-k>^\h+$<ret> Hd >
    # indent after an opening brace or parenthesis at end of line
    try %< execute-keys -draft k <a-x> <a-k>[{(]\h*$<ret> j <a-gt> >
    # indent after a label
    try %< execute-keys -draft k <a-x> s[a-zA-Z0-9_-]+:\h*$<ret> j <a-gt> >
    # indent after a statement not followed by an opening brace
    try %< execute-keys -draft k <a-x> s\)\h*(?://[^\n]+)?\n\z<ret> \
                               <a-semicolon>mB <a-k>\A\b(if|for|while)\b<ret> <a-semicolon>j <a-gt> >
    try %< execute-keys -draft k <a-x> s \belse\b\h*(?://[^\n]+)?\n\z<ret> \
                               j <a-gt> >
    # deindent after a single line statement end
    try %< execute-keys -draft K <a-x> <a-k>\;\h*(//[^\n]+)?$<ret> \
                               K <a-x> s\)(\h+\w+)*\h*(//[^\n]+)?\n([^\n]*\n){2}\z<ret> \
                               MB <a-k>\A\b(if|for|while)\b<ret> <a-S>1<a-&> >
    try %< execute-keys -draft K <a-x> <a-k>\;\h*(//[^\n]+)?$<ret> \
                               K <a-x> s \belse\b\h*(?://[^\n]+)?\n([^\n]*\n){2}\z<ret> \
                               <a-S>1<a-&> >
    # deindent closing brace(s) when after cursor
    try %< execute-keys -draft <a-x> <a-k> ^\h*[})] <ret> gh / [})] <esc> m <a-S> 1<a-&> >
    # align to the opening parenthesis or opening brace (whichever is first)
    # on a previous line if its followed by text on the same line
    try %< evaluate-commands -draft %<
        # Go to opening parenthesis and opening brace, then select the most nested one
        try %< execute-keys [c [({],[)}] <ret> >
        # Validate selection and get first and last char
        execute-keys <a-k>\A[{(](\h*\S+)+\n<ret> <a-K>"(([^"]*"){2})*<ret> <a-K>'(([^']*'){2})*<ret> <a-:><a-semicolon>L <a-S>
        # Remove possibly incorrect indent from new line which was copied from previous line
        try %< execute-keys -draft <space> <a-h> s\h+<ret> d >
        # Now indent and align that new line with the opening parenthesis/brace
        execute-keys 1<a-&> &
     > >
> >

define-command -hidden c-family-indent-on-opening-curly-brace %[
    # align indent with opening paren when { is entered on a new line after the closing paren
    try %[ execute-keys -draft -itersel h<a-F>)M <a-k> \A\(.*\)\h*\n\h*\{\z <ret> <a-S> 1<a-&> ]
    # align indent with opening paren when { is entered on a new line after the else
    try %[ execute-keys -draft -itersel hK <a-x> s \belse\b\h*(?://[^\n]+)?\n\h*\{<ret> <a-S> 1<a-&> ]
]

define-command -hidden c-family-indent-on-closing-curly-brace %[
    # align to opening curly brace when alone on a line
    try %[
        # in case open curly brace follows a closing paren, align indent with opening paren
        execute-keys -itersel -draft <a-h><a-:><a-k>^\h+\}$<ret>hm <a-F>)M <a-k> \A\(.*\)\h\{.*\}\z <ret> <a-S>1<a-&>
    ] catch %[
        # otherwise align with open curly brace
        execute-keys -itersel -draft <a-h><a-:><a-k>^\h+\}$<ret>hm<a-S>1<a-&>
    ] catch %[]
]

define-command -hidden c-family-insert-on-closing-curly-brace %[
    # add a semicolon after a closing brace if part of a class, union or struct definition
    try %[ execute-keys -itersel -draft hm<a-x>B<a-x><a-k>\A\h*(class|struct|union|enum)<ret> '<a-;>;i;<esc>' ]
]

define-command -hidden c-family-insert-on-newline %[ evaluate-commands -itersel -draft %[
    execute-keys <semicolon>
    try %[
        evaluate-commands -draft -save-regs '/"' %[
            # copy the commenting prefix
            execute-keys -save-regs '' k <a-x>1s^\h*(//+\h*)<ret> y
            try %[
                # if the previous comment isn't empty, create a new one
                execute-keys <a-x><a-K>^\h*//+\h*$<ret> j<a-x>s^\h*<ret>P
            ] catch %[
                # if there is no text in the previous comment, remove it completely
                execute-keys d
            ]
        ]
    ]
    try %[
        # if the previous line isn't within a comment scope, break
        execute-keys -draft k<a-x> <a-k>^(\h*/\*|\h+\*(?!/))<ret>

        # find comment opening, validate it was not closed, and check its using star prefixes
        execute-keys -draft <a-?>/\*<ret><a-H> <a-K>\*/<ret> <a-k>\A\h*/\*([^\n]*\n\h*\*)*[^\n]*\n\h*.\z<ret>

        try %[
            # if the previous line is opening the comment, insert star preceeded by space
            execute-keys -draft k<a-x><a-k>^\h*/\*<ret>
            execute-keys -draft i*<space><esc>
        ] catch %[
           try %[
                # if the next line is a comment line insert a star
                execute-keys -draft j<a-x><a-k>^\h+\*<ret>
                execute-keys -draft i*<space><esc>
            ] catch %[
                try %[
                    # if the previous line is an empty comment line, close the comment scope
                    execute-keys -draft k<a-x><a-k>^\h+\*\h+$<ret> <a-x>1s\*(\h*)<ret>c/<esc>
                ] catch %[
                    # if the previous line is a non-empty comment line, add a star
                    execute-keys -draft i*<space><esc>
                ]
            ]
        ]

        # trim trailing whitespace on the previous line
        try %[ execute-keys -draft s\h+$<ret> d ]
        # align the new star with the previous one
        execute-keys K<a-x>1s^[^*]*(\*)<ret>&
    ]
] ]

# Regions definition are the same between c++ and objective-c
evaluate-commands %sh{
    for ft in c cpp objc; do
        if [ "${ft}" = "objc" ]; then
            maybe_at='@?'
        else
            maybe_at=''
        fi

        cat <<-EOF
            add-highlighter shared/$ft regions
            add-highlighter shared/$ft/code default-region group
            add-highlighter shared/$ft/string region %{$maybe_at(?<!')(?<!'\\\\)"} %{(?<!\\\\)(?:\\\\\\\\)*"} fill string
            add-highlighter shared/$ft/raw_string region -match-capture %{R"([^(]*)\\(} %{\\)([^")]*)"} fill string
            add-highlighter shared/$ft/javadoc region /\*\*[^/] \*/ fill documentation
            add-highlighter shared/$ft/qtdoc region /\*! \*/ fill documentation
            add-highlighter shared/$ft/inline_doc region /// $ fill documentation
            add-highlighter shared/$ft/inline_qtdoc region //! $ fill documentation
            add-highlighter shared/$ft/comment region /\\* \\*/ fill comment
            add-highlighter shared/$ft/line_comment region // (?<!\\\\)(?=\\n) fill comment
            add-highlighter shared/$ft/disabled region -recurse "#\\h*if(?:def)?" ^\\h*?#\\h*if\\h+(?:0|FALSE)\\b "#\\h*(?:else|elif|endif)" fill comment
            add-highlighter shared/$ft/macro region %{^\\h*?\\K#} %{(?<!\\\\)(?=\\n)|(?=//)} group

            add-highlighter shared/$ft/macro/ fill meta
            add-highlighter shared/$ft/macro/ regex ^\\h*#\\h*include\\h+(\\S*) 1:module
            add-highlighter shared/$ft/macro/ regex /\\*.*?\\*/ 0:comment
	EOF
    done
}

# c specific
add-highlighter shared/c/code/numbers regex %{\b-?(0x[0-9a-fA-F]+|\d+)([uU][lL]{0,2}|[lL]{1,2}[uU]?|[fFdDiI]|([eE][-+]?\d+))?|'((\\.)?|[^'\\])'} 0:value
evaluate-commands %sh{
    # Grammar
    keywords='asm break case continue default do else for goto if return
              sizeof switch while offsetof alignas alignof'
    attributes='auto atomic const enum extern inline register restrict static
                struct typedef union volatile thread_local'
    types='char double float int long short signed unsigned void
           complex imaginary
           fenv_t fexcept_t
           imaxdiv_t
           lconv
           float_t double_t
           jmp_buf
           sig_atomic_t
           va_list
           memory_order atomic_flag atomic_bool atomic_char atomic_schar atomic_uchar atomic_wchar atomic_short atomic_ushort atomic_int atomic_uint atomic_long atomic_llong atomic_ulong atomic_ullong atomic_char16_t atomic_char32_t atomic_intptr_t atomic_intmax_t atomic_int8_t atomic_int16_t atomic_int32_t atomic_int64_t atomic_int_least8_t atomic_int_least16_t atomic_int_least32_t atomic_int_least64_t atomic_int_fast8_t atomic_int_fast16_t atomic_int_fast32_t atomic_int_fast64_t atomic_uintptr_t atomic_uintmax_t atomic_uint8_t atomic_uint16_t atomic_uint32_t atomic_uint64_t atomic_uint_least8_t atomic_uint_least16_t atomic_uint_least32_t atomic_uint_least64_t atomic_uint_fast8_t atomic_uint_fast16_t atomic_uint_fast32_t atomic_uint_fast64_t atomic_size_t atomic_ptrdiff_t
           bool
           ptrdiff_t size_t max_align_t wchar_t
           intptr_t intmax_t int8_t int16_t int32_t int64_t int_least8_t int_least16_t int_least32_t int_least64_t int_fast8_t int_fast16_t int_fast32_t int_fast64_t uintptr_t uintmax_t uint8_t uint16_t uint32_t uint64_t uint_least8_t uint_least16_t uint_least32_t uint_least64_t uint_fast8_t uint_fast16_t uint_fast32_t uint_fast64_t
           FILE fpos_t
           div_t ldiv_t lldiv_t
           cnd_t thrd_t thrd_start_t tss_t tss_dtor_t mtx_t once_flag
           clock_t time_t timespec tm
           mbstate_t wint_t
           wctrans_t wctype_t
           char16_t char32_t
           ssize_t gid_t uid_t off_t off64_t useconds_t pid_t socklen_t'

    macros='assert static_assert NDEBUG
            I
            EDOM EILSEQ ERANGE errno
            FE_DIVBYZERO FE_INEXACT FE_INVALID FE_OVERFLOW FE_UNDERFLOW FE_ALL_EXCEPT FE_DOWNWARD FE_TONEAREST FE_TOWARDZERO FE_UPWARD FE_DFL_ENV
            DECIMAL_DIG FLT_ROUNDS FLT_EVAL_METHOD FLT_RADIX FLT_DIG FLT_MANT_DIG FLT_DECIMAL_DIG FLT_MIN_EXP FLT_MIN_10_EXP FLT_MAX_EXP FLT_MAX FLT_EPSILON FLT_TRUE_MIN FLT_HAS_SUBNORM DBL_DIG DBL_MANT_DIG DBL_DECIMAL_DIG DBL_MIN_EXP DBL_MIN_10_EXP DBL_MAX_EXP DBL_MAX DBL_EPSILON DBL_TRUE_MIN DBL_HAS_SUBNORM LDBL_DIG LDBL_MANT_DIG LDBL_DECIMAL_DIG LDBL_MIN_EXP LDBL_MIN_10_EXP LDBL_MAX_EXP LDBL_MAX LDBL_EPSILON LDBL_TRUE_MIN LDBL_HAS_SUBNORM
            PRIdMAX PRIdPTR PRId8 PRId16 PRId32 PRId64 PRIdLEAST8 PRIdLEAST16 PRIdLEAST32 PRIdLEAST64 PRIdFAST8 PRIdFAST16 PRIdFAST32 PRIdFAST64 PRIiMAX PRIiPTR PRIi8 PRIi16 PRIi32 PRIi64 PRIiLEAST8 PRIiLEAST16 PRIiLEAST32 PRIiLEAST64 PRIiFAST8 PRIiFAST16 PRIiFAST32 PRIiFAST64 PRIoMAX PRIoPTR PRIo8 PRIo16 PRIo32 PRIo64 PRIoLEAST8 PRIoLEAST16 PRIoLEAST32 PRIoLEAST64 PRIoFAST8 PRIoFAST16 PRIoFAST32 PRIoFAST64 PRIuMAX PRIuPTR PRIu8 PRIu16 PRIu32 PRIu64 PRIuLEAST8 PRIuLEAST16 PRIuLEAST32 PRIuLEAST64 PRIuFAST8 PRIuFAST16 PRIuFAST32 PRIuFAST64 PRIxMAX PRIxPTR PRIx8 PRIx16 PRIx32 PRIx64 PRIxLEAST8 PRIxLEAST16 PRIxLEAST32 PRIxLEAST64 PRIxFAST8 PRIxFAST16 PRIxFAST32 PRIxFAST64 PRIXMAX PRIXPTR PRIX8 PRIX16 PRIX32 PRIX64 PRIXLEAST8 PRIXLEAST16 PRIXLEAST32 PRIXLEAST64 PRIXFAST8 PRIXFAST16 PRIXFAST32 PRIXFAST64 SCNdMAX SCNdPTR SCNd8 SCNd16 SCNd32 SCNd64 SCNdLEAST8 SCNdLEAST16 SCNdLEAST32 SCNdLEAST64 SCNdFAST8 SCNdFAST16 SCNdFAST32 SCNdFAST64 SCNiMAX SCNiPTR SCNi8 SCNi16 SCNi32 SCNi64 SCNiLEAST8 SCNiLEAST16 SCNiLEAST32 SCNiLEAST64 SCNiFAST8 SCNiFAST16 SCNiFAST32 SCNiFAST64 SCNoMAX SCNoPTR SCNo8 SCNo16 SCNo32 SCNo64 SCNoLEAST8 SCNoLEAST16 SCNoLEAST32 SCNoLEAST64 SCNoFAST8 SCNoFAST16 SCNoFAST32 SCNoFAST64 SCNuMAX SCNuPTR SCNu8 SCNu16 SCNu32 SCNu64 SCNuLEAST8 SCNuLEAST16 SCNuLEAST32 SCNuLEAST64 SCNuFAST8 SCNuFAST16 SCNuFAST32 SCNuFAST64 SCNxMAX SCNxPTR SCNx8 SCNx16 SCNx32 SCNx64 SCNxLEAST8 SCNxLEAST16 SCNxLEAST32 SCNxLEAST64 SCNxFAST8 SCNxFAST16 SCNxFAST32 SCNxFAST64
            and and_eq bitand bitor compl not not_eq or or_eq xor xor_eq
            CHAR_MIN CHAR_MAX SCHAR_MIN SCHAR_MAX WCHAR_MIN WCHAR_MAX SHRT_MIN SHRT_MAX INT_MIN INT_MAX LONG_MIN LONG_MAX LLONG_MIN LLONG_MAX MB_LEN_MAX UCHAR_MAX USHRT_MAX UINT_MAX ULONG_MAX ULLONG_MAX CHAR_BIT
            LC_ALL LC_COLLATE LC_CTYPE LC_MONETARY LC_NUMERIC LC_TIME
            HUGE_VAL HUGE_VALF HUGE_VALL INFINITY NAN FP_INFINITE FP_NAN FP_NORMAL FP_SUBNORMAL FP_ZERO FP_FAST_FMA FP_FAST_FMAF FP_FAST_FMAL FP_ILOGB0 FP_ILOGBNAN MATH_ERRNO MATH_ERREXCEPT math_errhandling isgreater isgreaterequal isless islessequal islessgreater isunordered
            setjmp
            SIG_DFL SIG_ERR SIG_IGN SIGABRT SIGFPE SIGILL SIGINT SIGSEGV SIGTERM
            va_start va_arg va_end va_copy
            ATOMIC_BOOL_LOCK_FREE ATOMIC_CHAR_LOCK_FREE ATOMIC_CHAR16_T_LOCK_FREE ATOMIC_CHAR32_T_LOCK_FREE ATOMIC_WCHAR_T_LOCK_FREE ATOMIC_SHORT_LOCK_FREE ATOMIC_INT_LOCK_FREE ATOMIC_LONG_LOCK_FREE ATOMIC_LLONG_LOCK_FREE ATOMIC_POINTER_LOCK_FREE ATOMIC_FLAG_INIT ATOMIC_VAR_INIT memory_order_relaxed memory_order_consume memory_order_acquire memory_order_release memory_order_acq_rel memory_order_seq_cst kill_dependency
            true false
            NULL
            _IOFBF _IOLBF _IONBF BUFSIZ EOF FOPEN_MAX FILENAME_MAX TMP_MAX L_tmpnam SEEK_CUR SEEK_END SEEK_SET stderr stdin stdout
            EXIT_FAILURE EXIT_SUCCESS MB_CUR_MAX RAND_MAX
            PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX WINT_MIN WINT_MAX INTMAX_MIN INTMAX_MAX INTPTR_MIN INTPTR_MAX INT8_MIN INT8_MAX INT16_MIN INT16_MAX INT32_MIN INT32_MAX INT64_MIN INT64_MAX INT_LEAST8_MIN INT_LEAST8_MAX INT_LEAST16_MIN INT_LEAST16_MAX INT_LEAST32_MIN INT_LEAST32_MAX INT_LEAST64_MIN INT_LEAST64_MAX INT_FAST8_MIN INT_FAST8_MAX INT_FAST16_MIN INT_FAST16_MAX INT_FAST32_MIN INT_FAST32_MAX INT_FAST64_MIN INT_FAST64_MAX UINTMAX_MAX UINTPTR_MAX UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX UINT_LEAST8_MAX UINT_LEAST16_MAX UINT_LEAST32_MAX UINT_LEAST64_MAX UINT_FAST8_MAX UINT_FAST16_MAX UINT_FAST32_MAX UINT_FAST64_MAX INTMAX_C INT8_C INT16_C INT32_C INT64_C UINTMAX_C UINT8_C UINT16_C UINT32_C UINT64_C
            mtx_plain mtx_recursive mtx_timed thrd_timedout thrd_success thrd_busy thrd_error thrd_nomem ONCE_FLAG_INIT TSS_DTOR_ITERATION
            CLOCKS_PER_SEC TIME_UTC
            WEOF
            noreturn
            R_OK W_OK X_OK F_OK F_LOCK F_ULOCK F_TLOCK F_TEST'

    join() { sep=$2; eval set -- $1; IFS="$sep"; echo "$*"; }

    # Add the language's grammar to the static completion list
    printf %s\\n "declare-option str-list c_static_words $(join "${keywords} ${attributes} ${types} ${macros}" ' ')"

    # Highlight keywords
    printf %s "
        add-highlighter shared/c/code/keywords regex \b($(join "${keywords}" '|'))\b 0:keyword
        add-highlighter shared/c/code/attributes regex \b($(join "${attributes}" '|'))\b 0:attribute
        add-highlighter shared/c/code/types regex \b($(join "${types}" '|'))\b 0:type
        add-highlighter shared/c/code/values regex \b($(join "${macros}" '|'))\b 0:value
    "
}

# c++ specific

# integer literals
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b[1-9]('?\d+)*(ul?l?|ll?u?)?\b(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b0b[01]('?[01]+)*(ul?l?|ll?u?)?\b(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b0('?[0-7]+)*(ul?l?|ll?u?)?\b(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b0x[\da-f]('?[\da-f]+)*(ul?l?|ll?u?)?\b(?!\.)} 0:value

# floating point literals
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b\d('?\d+)*\.([fl]\b|\B)(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b\d('?\d+)*\.?e[+-]?\d('?\d+)*[fl]?\b(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)(\b(\d('?\d+)*)|\B)\.\d('?[\d]+)*(e[+-]?\d('?\d+)*)?[fl]?\b(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b0x[\da-f]('?[\da-f]+)*\.([fl]\b|\B)(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b0x[\da-f]('?[\da-f]+)*\.?p[+-]?\d('?\d+)*)?[fl]?\b(?!\.)} 0:value
add-highlighter shared/cpp/code/ regex %{(?i)(?<!\.)\b0x([\da-f]('?[\da-f]+)*)?\.\d('?[\d]+)*(p[+-]?\d('?\d+)*)?[fl]?\b(?!\.)} 0:value

# character literals (no multi-character literals)
add-highlighter shared/cpp/code/char regex %{(\b(u8|u|U|L)|\B)'((\\.)|[^'\\])'\B} 0:value

evaluate-commands %sh{
    # Grammar
    keywords='alignas alignof and and_eq asm bitand bitor break case catch
              compl const_cast continue decltype delete do dynamic_cast
              else export for goto if new not not_eq operator or or_eq
              reinterpret_cast return sizeof static_assert static_cast switch
              throw try typedef typeid using while xor xor_eq'
    attributes='audit auto axiom const consteval constexpr default explicit
                extern final friend inline mutable noexcept override private
                protected public register requires static thread_local typename
                virtual volatile'
    entities='class concept enum namespace struct template union'
    types='bool byte char char8_t char16_t char32_t double float int long
           max_align_t nullptr_t ptrdiff_t short signed size_t unsigned void
           wchar_t'
    values='NULL false nullptr this true'

    join() { sep=$2; eval set -- $1; IFS="$sep"; echo "$*"; }

    # Add the language's grammar to the static completion list
    printf %s\\n "declare-option str-list cpp_static_words $(join "${keywords} ${attributes} ${entities} ${types} ${values}" ' ')"

    # Highlight keywords
    printf %s "
        add-highlighter shared/cpp/code/keywords regex \b($(join "${keywords}" '|'))\b 0:keyword
        add-highlighter shared/cpp/code/attributes regex \b($(join "${attributes} ${entities}" '|'))\b 0:attribute
        add-highlighter shared/cpp/code/types regex \b($(join "${types}" '|'))\b 0:type
        add-highlighter shared/cpp/code/values regex \b($(join "${values}" '|'))\b 0:value
    "
}

# c and c++ compiler macros
evaluate-commands %sh{
    builtin_macros="__cplusplus|__STDC_HOSTED__|__FILE__|__LINE__|__DATE__|__TIME__|__STDCPP_DEFAULT_NEW_ALIGNMENT__"

    printf %s "
        add-highlighter shared/c/code/macros regex \b(${builtin_macros})\b 0:builtin
        add-highlighter shared/cpp/code/macros regex \b(${builtin_macros})\b 0:builtin
    "
}

# objective-c specific
add-highlighter shared/objc/code/number regex %{\b-?\d+[fdiu]?|'((\\.)?|[^'\\])'} 0:value

evaluate-commands %sh{
    # Grammar
    keywords='break case continue default do else for goto if return switch
              while'
    attributes='IBAction IBOutlet __block assign auto const copy enum extern
                inline nonatomic readonly retain static strong struct typedef
                union volatile weak'
    types='BOOL CGFloat NSInteger NSString NSUInteger bool char float
           instancetype int long short signed size_t unsigned void'
    values='FALSE NO NULL TRUE YES id nil self super'
    decorators='autoreleasepool catch class end implementation interface
                property protocol selector synchronized synthesize try'

    join() { sep=$2; eval set -- $1; IFS="$sep"; echo "$*"; }

    # Add the language's grammar to the static completion list
    printf %s\\n "declare-option str-list objc_static_words $(join "${keywords} ${attributes} ${types} ${values} ${decorators}" ' ')"

    # Highlight keywords
    printf %s "
        add-highlighter shared/objc/code/keywords regex \b($(join "${keywords}" '|'))\b 0:keyword
        add-highlighter shared/objc/code/attributes regex \b($(join "${attributes}" '|'))\b 0:attribute
        add-highlighter shared/objc/code/types regex \b($(join "${types}" '|'))\b 0:type
        add-highlighter shared/objc/code/values regex \b($(join "${values}" '|'))\b 0:value
        add-highlighter shared/objc/code/decorators regex  @($(join "${decorators}" '|'))\b 0:attribute
    "
}

declare-option -docstring %{
    control the type of include guard to be inserted in empty headers
    Can be one of the following:
        ifdef: old style ifndef/define guard
        pragma: newer type of guard using "pragma once"
} str c_include_guard_style "ifdef"

define-command -hidden c-family-insert-include-guards %{
    evaluate-commands %sh{
        case "${kak_opt_c_include_guard_style}" in
            ifdef)
                echo 'execute-keys ggi<c-r>%<ret><esc>ggxs\.<ret>c_<esc><space>A_INCLUDED<esc>ggxyppI#ifndef<space><esc>jI#define<space><esc>jI#endif<space>//<space><esc>O<esc>'
                ;;
            pragma)
                echo 'execute-keys ggi#pragma<space>once<esc>'
                ;;
            *);;
        esac
    }
}

hook -group c-family-insert global BufNewFile .*\.(h|hh|hpp|hxx|H) c-family-insert-include-guards

declare-option -docstring "colon separated list of path in which header files will be looked for" \
    str-list alt_dirs '.' '..'

define-command -hidden c-family-alternative-file %{
    evaluate-commands %sh{
        file="${kak_buffile##*/}"
        file_noext="${file%.*}"
        dir=$(dirname "${kak_buffile}")

        # Set $@ to alt_dirs
        eval "set -- ${kak_quoted_opt_alt_dirs}"

        case ${file} in
            *.c|*.cc|*.cpp|*.cxx|*.C|*.inl|*.m)
                for alt_dir in "$@"; do
                    for ext in h hh hpp hxx H; do
                        altname="${dir}/${alt_dir}/${file_noext}.${ext}"
                        if [ -f ${altname} ]; then
                            printf 'edit %%{%s}\n' "${altname}"
                            exit
                        fi
                    done
                done
            ;;
            *.h|*.hh|*.hpp|*.hxx|*.H)
                for alt_dir in "$@"; do
                    for ext in c cc cpp cxx C m; do
                        altname="${dir}/${alt_dir}/${file_noext}.${ext}"
                        if [ -f ${altname} ]; then
                            printf 'edit %%{%s}\n' "${altname}"
                            exit
                        fi
                    done
                done
            ;;
            *)
                echo "fail 'extension not recognized'"
                exit
            ;;
        esac
        echo "fail 'alternative file not found'"
    }
}

define-command c-alternative-file -docstring "Jump to the alternate c file (header/implementation)" %{
    c-family-alternative-file
}
define-command cpp-alternative-file -docstring "Jump to the alternate cpp file (header/implementation)" %{
    c-family-alternative-file
}
define-command objc-alternative-file -docstring "Jump to the alternate objc file (header/implementation)" %{
    c-family-alternative-file
}

§

# Module aliases
provide-module c %{ require-module c-family }
provide-module cpp %{ require-module c-family }
provide-module objc %{ require-module c-family }
