#`[
  Describes a 4-digit version number.
]
grammar dynaver {
    rule TOP { <number> <identifier>? <metadata>? }
    
    token number { ( <digit>+ ) "." ( <digit>+ ) [ "." ( <digit>+ ) [ "." ( <digit>+ ) ]? ]? }
    token identifier { [ <pre> <post>? | <post> <pre>? ] }
    token metadata { '+' ( <metadata_char>+ ) }
    
    token pre  { "-" ( <pre_char>+ ) }
    token post { "_" ( <post_char>+ ) }
    
    token pre_char { <[a..zA..Z0..9.-]> }
    token post_char { <[a..zA..Z0..9._]> }
    token metadata_char { <[a..zA..Z0..9._-]> }
    
    token digit { \d } #built-in
}
