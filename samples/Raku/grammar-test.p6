token pod_formatting_code {
    $<code>=<[A..Z]>
    '<' { $*POD_IN_FORMATTINGCODE := 1 }
    $<content>=[ <!before '>'> <pod_string_character> ]+
    '>' { $*POD_IN_FORMATTINGCODE := 0 }
}

token pod_string {
    <pod_string_character>+
}

token something:sym«<» {
    <!>
}

token name {
    <!>
}

token comment:sym<#> {
   '#' {} \N*
}
