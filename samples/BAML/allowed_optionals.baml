class OptionalListAndMap {
  p string[]?
  q map<string, string>?
}

function AllowedOptionals(optionals: OptionalListAndMap) -> OptionalListAndMap {
  client GPT4
  prompt #"
    Return the given input:

    {{optionals}}

    {{ctx.output_format}}
  "#
}