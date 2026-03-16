(assignment_expression
  left: (member_expression
    object: (call_expression)))

(class_declaration
  name: (identifier) @class_name
  !type_parameters)

(binary_expression
  operator: "!="
  right: (null))

(MISSING identifier) @missing-identifier
(MISSING ";") @missing-semicolon

(assignment_expression
  left: (identifier) @the-function-name
  right: (function))

(comment)+

[
  "break"
  "delete"
  "else"
  "for"
  "function"
  "if"
  "return"
  "try"
  "while"
] @keyword

(body
  [
   (identifier)
   (function)
   ]*
)

(class_declaration
  (decorator)* @the-decorator
  name: (identifier) @the-name)

(call_expression
  function: (identifier) @the-function
  arguments: (arguments (string)? @the-string-arg))

(dotted_name
  (identifier) @prev-id
  .
  (identifier) @next-id)

(
  (number)
  ("," (number))*
)
