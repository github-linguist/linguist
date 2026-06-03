declare-option -docstring "name of the client in which documentation is to be displayed" \
    str docsclient

declare-option -hidden range-specs doc_render_ranges
declare-option -hidden range-specs doc_render_links
declare-option -hidden range-specs doc_links
declare-option -hidden range-specs doc_anchors

define-command -hidden -params 4 doc-render-regex %{
    evaluate-commands -draft %{ try %{
        execute-keys <percent> s %arg{1} <ret>
        execute-keys -draft s %arg{2} <ret> d
        execute-keys "%arg{3}"
        evaluate-commands %sh{
            face="$4"
            eval "set -- $kak_quoted_selections_desc"
            ranges=""
            for desc in "$@"; do ranges="$ranges '$desc|$face'"; done
            echo "update-option buffer doc_render_ranges"
            echo "set-option -add buffer doc_render_ranges $ranges"
        }
    } }
}

define-command -hidden doc-parse-links %{
    evaluate-commands -draft %{ try %{
        execute-keys <percent> s <lt><lt>(.*?),.*?<gt><gt> <ret>
        execute-keys -draft s <lt><lt>.*,|<gt><gt> <ret> d
        execute-keys H
        set-option buffer doc_links %val{timestamp}
        set-option buffer doc_render_links %val{timestamp}
        evaluate-commands -itersel %{
            set-option -add buffer doc_links "%val{selection_desc}|%reg{1}"
            set-option -add buffer doc_render_links "%val{selection_desc}|default+u"
        }
    } }
}

define-command -hidden doc-parse-anchors %{
    evaluate-commands -draft %{ try %{
        set-option buffer doc_anchors %val{timestamp}
        # Find sections as add them as imlicit anchors
        execute-keys <percent> s ^={2,}\h+([^\n]+)$ <ret>
        evaluate-commands -itersel %{
            set-option -add buffer doc_anchors "%val{selection_desc}|%sh{printf '%s' ""$kak_main_reg_1"" | tr '[A-Z ]' '[a-z-]'}"
        }

        # Parse explicit anchors and remove their text
        execute-keys <percent> s \[\[(.*?)\]\]\s* <ret>
        evaluate-commands -itersel %{
            set-option -add buffer doc_anchors "%val{selection_desc}|%reg{1}"
        }
        execute-keys d
        update-option buffer doc_anchors
    } }
}

define-command doc-jump-to-anchor -params 1 %{
    update-option buffer doc_anchors
    evaluate-commands %sh{
        anchor="$1"
        eval "set -- $kak_quoted_opt_doc_anchors"

        shift
        for range in "$@"; do
            if [ "${range#*|}" = "$anchor" ]; then
                printf '%s\n'  "select '${range%|*}'; execute-keys vv"
                exit
            fi
        done
        printf "fail No such anchor '%s'\n" "${anchor}"
    }
}

define-command doc-follow-link %{
    update-option buffer doc_links
    evaluate-commands %sh{
        eval "set -- $kak_quoted_opt_doc_links"
        for link in "$@"; do
            printf '%s\n' "$link"
        done | awk -v FS='[.,|#]' '
            BEGIN {
                l=ENVIRON["kak_cursor_line"];
                c=ENVIRON["kak_cursor_column"];
            }
            l >= $1 && c >= $2 && l <= $3 && c <= $4 {
                if (NF == 6) {
                    print "doc " $5
                    if ($6 != "") {
                        print "doc-jump-to-anchor %{" $6 "}"
                    }
                } else {
                    print "doc-jump-to-anchor %{" $5 "}"
                }
                exit
            }
        '
    }
}

define-command -params 1 -hidden doc-render %{
    edit! -scratch "*doc-%sh{basename $1 .asciidoc}*"
    execute-keys "!cat %arg{1}<ret>gg"

    doc-parse-anchors

    # Join paragraphs together
    try %{
        execute-keys -draft '%S\n{2,}|(?<lt>=\+)\n|^[^\n]+::\n|^\h*[*-]\h+<ret>' \
            <a-K>^\h*-{2,}(\n|\z)<ret> S\n\z<ret> <a-k>\n<ret> <a-j>
    }

    # Remove some line end markers
    try %{ execute-keys -draft <percent> s \h*(\+|:{2,})$ <ret> d }

    # Setup the doc_render_ranges option
    set-option buffer doc_render_ranges %val{timestamp}
    doc-render-regex \B(?<!\\)\*(?=\S)[^\n]+?(?<=\S)(?<!\\)\*\B \A|.\z 'H' default+b
    doc-render-regex \b(?<!\\)_(?=\S)[^\n]+?(?<=\S)(?<!\\)_\b \A|.\z 'H' default+i
    doc-render-regex \B(?<!\\)`(?=\S)[^\n]+?(?<=\S)(?<!\\)`\B \A|.\z 'H' mono
    doc-render-regex ^=\h+[^\n]+ ^=\h+ '~' title
    doc-render-regex ^={2,}\h+[^\n]+ ^={2,}\h+ '' header
    doc-render-regex ^\h*-{2,}\n\h*.*?^\h*-{2,}\n ^\h*-{2,}\n '' block

    doc-parse-links

    # Remove escaping of * and `
    try %{ execute-keys -draft <percent> s \\((?=\*)|(?=`)) <ret> d }

    set-option buffer readonly true
    add-highlighter buffer/ ranges doc_render_ranges
    add-highlighter buffer/ ranges doc_render_links
    add-highlighter buffer/ wrap -word -indent
    map buffer normal <ret> ': doc-follow-link<ret>'
}

define-command -params 1..2 \
    -shell-script-candidates %{
        if [ "$kak_token_to_complete" -eq 0 ]; then
            find -L \
                "${kak_config}/autoload/" \
                "${kak_runtime}/doc/" \
                "${kak_runtime}/rc/" \
                -type f -name "*.asciidoc" |
                sed 's,.*/,,; s/\.[^.]*$//'
        elif [ "$kak_token_to_complete" -eq 1 ]; then
            page=$(
                find -L \
                    "${kak_config}/autoload/" \
                    "${kak_runtime}/doc/" \
                    "${kak_runtime}/rc/" \
                    -type f -name "$1.asciidoc" |
                    head -1
            )
            if [ -f "${page}" ]; then
                awk '
                    /^==+ +/ { sub(/^==+ +/, ""); print }
                    /^\[\[[^\]]+\]\]/ { sub(/^\[\[/, ""); sub(/\]\].*/, ""); print }
                ' < $page | tr '[A-Z ]' '[a-z-]'
            fi
        fi
    } \
    doc -docstring %{
        doc <topic> [<keyword>]: open a buffer containing documentation about a given topic
        An optional keyword argument can be passed to the function, which will be automatically selected in the documentation

        See `:doc doc` for details.
    } %{
    evaluate-commands %sh{
        page=$(
            find -L \
                "${kak_config}/autoload/" \
                "${kak_runtime}/doc/" \
                "${kak_runtime}/rc/" \
                -type f -name "$1.asciidoc" |
                head -1
        )
        if [ -f "${page}" ]; then
            jump_cmd=""
            if [ $# -eq 2 ]; then
                jump_cmd="doc-jump-to-anchor '$2'"
            fi
            printf %s\\n "evaluate-commands -try-client %opt{docsclient} %{ doc-render ${page}; ${jump_cmd} }"
        else
            printf 'fail No such doc file: %s\n' "${page}"
        fi
    }
}

alias global help doc

