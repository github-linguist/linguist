## Add Azalea Language Support

This PR adds support for the Azalea programming language to GitHub Linguist.

### Changes

- Added Azalea to `languages.yml` with `.az` extension
- Added TextMate grammar for syntax highlighting
- Added sample file demonstrating Azalea syntax
- Registered grammar in `grammars.yml`

### About Azalea

Azalea is an interpreted programming language with a minimal, Latin-influenced syntax. It supports flexible syntax patterns, CSS styling capabilities, and is suitable for both UI and backend development.

### Grammar Coverage

The TextMate grammar provides syntax highlighting for:
- Keywords (form, act, call, if, loop, etc.)
- Operators (plus, minus, times, div, over, under, etc.)
- Types (num, text, bool, list, map, void)
- Strings, numbers, and comments
- Function calls and UI components

### References

- Language repository: https://github.com/xazalea/language

