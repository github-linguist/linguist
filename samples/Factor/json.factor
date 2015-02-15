USING: json.writer json.reader ;

SYMBOL: foo

! Load a JSON string into a data structure
"[[\"foo\",1],[\"bar\",[10,\"apples\"]]]" json> foo set


! Create a new data structure and serialize into JSON
{ { "blue" { "ocean" "water" } } >json
