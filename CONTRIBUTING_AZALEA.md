# Add Azalea Language Support

This PR adds support for the Azalea programming language to GitHub Linguist.

## Changes

1. **Added Azalea to `languages.yml`**:
   - Language name: Azalea
   - File extension: `.az`
   - Color: `#667eea`
   - TextMate scope: `source.azalea`
   - Aliases: `azalea-lang`

2. **TextMate Grammar**:
   - Location: `vendor/grammars/azalea-language/azalea.tmLanguage.json`
   - Syntax highlighting for keywords, operators, types, strings, numbers, comments, and function calls

3. **Grammar Registration**:
   - Added entry to `grammars.yml`

4. **Sample File**:
   - `samples/Azalea/hello.az` - Example Azalea code

## About Azalea

Azalea is an interpreted programming language with a minimal, Latin-influenced syntax. It supports flexible syntax patterns, full CSS styling capabilities, and is suitable for both UI and backend development.

## References

- Language repository: https://github.com/xazalea/language

