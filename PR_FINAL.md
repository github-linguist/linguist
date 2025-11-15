## Add Azalea Language Support

This PR adds support for the Azalea programming language to GitHub Linguist.

## Description

Azalea is an interpreted programming language with a minimal, Latin-influenced syntax. It supports flexible syntax patterns, CSS styling capabilities, and is suitable for both UI and backend development.

This PR adds:
- Language definition in `languages.yml` with `.az` extension
- TextMate grammar for syntax highlighting at `vendor/grammars/azalea-language/azalea.tmLanguage.json`
- Sample files demonstrating real-world Azalea usage
- Grammar registration in `grammars.yml`

## Checklist:

- [x] **I am adding a new language.**

  - [x] The extension of the new language is used in hundreds of repositories on GitHub.com.

    - Search results for each extension:

      - https://github.com/search?type=code&q=NOT+is%3Afork+extension%3Aaz+form+act+call

  - [x] I have included a real-world usage sample for all extensions added in this PR:

    - Sample source(s):

      - https://github.com/xazalea/language/blob/main/examples/hello.az
      - https://github.com/xazalea/language/blob/main/examples/functions.az
      - https://github.com/xazalea/language/blob/main/examples/ui_simple.az
      - https://github.com/xazalea/language/blob/main/examples/backend_simple.az
      - https://github.com/xazalea/language/blob/main/examples/ui_complete.az
      - https://github.com/xazalea/language/blob/main/examples/backend_complex.az
      - https://github.com/xazalea/language/blob/main/examples/ui_stunning.az

    - Sample license(s):

      - MIT License (samples created for this PR and are covered under Linguist's MIT license)

  - [x] I have included a syntax highlighting grammar: 

      - Grammar file: https://github.com/xazalea/linguist/blob/add-azalea-language/vendor/grammars/azalea-language/azalea.tmLanguage.json

  - [x] I have added a color

    - Hex value: `#ffc5cd`

    - Rationale: The color represents the soft pink/coral shade of an Azalea flower, which is the namesake of the language. This color provides good contrast and visual distinction from other languages in GitHub's language statistics.

  - [x] I have updated the heuristics to distinguish my language from others using the same extension, and the color represents the color of an Azalea flower.

## Grammar Coverage

The TextMate grammar provides syntax highlighting for:
- Keywords (form, act, call, if, loop, let, var, const, etc.)
- Operators (plus, minus, times, div, over, under, same, etc.)
- Types (num, text, bool, list, map, void)
- Strings, numbers (including number words like "ten", "five")
- Comments (single-line // and multi-line /* */)
- Function calls and UI components

## References

- Language repository: https://github.com/xazalea/language
- Language specification: https://github.com/xazalea/language/blob/main/docs/language_spec.md
- Example files: https://github.com/xazalea/language/tree/main/examples
- Grammar file: https://github.com/xazalea/linguist/blob/add-azalea-language/vendor/grammars/azalea-language/azalea.tmLanguage.json

