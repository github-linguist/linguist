
class InputClass {
  key string @alias("color")
  key2 string
}


class InputClassNested {
  key string
  nested InputClass @alias("interesting-key")
}
 

function AliasedInputClass(input: InputClass) -> string {
  client GPT35
  prompt #"

    {{input}}

    This is a test. What's the name of the first json key above? Remember, tell me the key, not value.
  "#
}
 
function AliasedInputClass2(input: InputClass) -> string {
  client GPT35
  prompt #"

    {# making sure we can still access the original key #}
    {%if input.key == "tiger"%}
      Repeat this value back to me, and nothing else: {{input.key}}
    {%endif%}
  "#
}
 
 function AliasedInputClassNested(input: InputClassNested) -> string {
  client GPT35
  prompt #"
    {{ _.role("user")}}

    {{input}}

    This is a test. What's the name of the second json key above? Remember, tell me the key, not value.
  "#
 }


enum AliasedEnum {
  KEY_ONE @alias("tiger")
  KEY_TWO
}

function AliasedInputEnum(input: AliasedEnum) -> string {
  client GPT4o
  prompt #"
    {{ _.role("user")}}


    Write out this word only in your response, in lowercase:
    ---
    {{input}}
    ---
    Answer:
  "#
}


function AliasedInputList(input: AliasedEnum[]) -> string {
  client GPT35
  prompt #"
    {{ _.role("user")}}
    Given this array:
    ---
    {{input}}
    ---

    Return the first element in the array:
  "#
}

