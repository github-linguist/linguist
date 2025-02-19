((comment) @injection.content
  name: (identifier) @_name
  (#set! injection.language "comment"))


((regex_literal
  (_) @injection.content)
  (#set! injection.language "regex"))

(recipe_body
  !shebang
  (#set! injection.language "bash")
  (#set! injection.include-children)) @injection.content

(external_command
  (command_body) @injection.content
  (#set! injection.language "bash"))

(source_file
  (setting "shell" ":=" "[" (string) @_langstr
    (#match? @_langstr ".*(powershell|pwsh|cmd).*")
    (#set! injection.language "powershell"))
  [
    (recipe
      (recipe_body
        !shebang
        (#set! injection.include-children)) @injection.content)

    (assignment
      (expression
        (value
          (external_command
            (command_body) @injection.content))))
  ])

(source_file
  (setting "shell" ":=" "[" (string) @injection.language
    (#not-match? @injection.language ".*(powershell|pwsh|cmd).*"))
  [
    (recipe
      (recipe_body
        !shebang
        (#set! injection.include-children)) @injection.content)

    (assignment
      (expression
        (value
          (external_command
            (command_body) @injection.content))))
  ])

(recipe_body
  (shebang
    (language) @injection.language)
  (#not-any-of? @injection.language "python3" "nodejs" "node" "uv")
  (#set! injection.include-children)) @injection.content

(recipe_body
  (shebang
    (language) @_lang)
  (#any-of? @_lang "python3" "uv")
  (#set! injection.language "python")
  (#set! injection.include-children)) @injection.content

(recipe_body
  (shebang
    (language) @_lang)
  (#any-of? @_lang "node" "nodejs")
  (#set! injection.language "javascript")
  (#set! injection.include-children)) @injection.content

(recipe_body
  (shebang) @injection.shebang
  (#set! injection.include-children)) @injection.content

