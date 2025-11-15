# Adding Azalea Language Support

This PR adds support for the Azalea programming language to GitHub Linguist.

## Changes Made

1. **Added Azalea to `languages.yml`**:
   - Language name: Azalea
   - File extension: `.az`
   - Color: `#667eea` (purple/blue gradient)
   - TextMate scope: `source.azalea`
   - Aliases: `azalea-lang`

2. **Created TextMate Grammar**:
   - Location: `vendor/grammars/azalea-language/azalea.tmLanguage.json`
   - Provides syntax highlighting for:
     - Keywords (form, act, call, if, loop, etc.)
     - Operators (plus, minus, times, div, over, under, etc.)
     - Types (num, text, bool, list, map, void)
     - Strings, numbers, comments
     - Function calls

3. **Added Grammar to `grammars.yml`**:
   - Registered the TextMate grammar for use by Linguist

4. **Added Sample File**:
   - `samples/Azalea/hello.az` - Example Azalea code

## About Azalea

Azalea is an elegant, minimal, powerful interpreted programming language designed to be:
- Extremely easy for beginners
- Extremely powerful for experts
- Perfect for UI development
- Great for backend development
- Hyper efficient

Key features:
- Flexible syntax (multiple ways to write the same code)
- Full CSS support for stunning UIs
- HTML-like components
- Minimal symbols: only `. , / ? ; !`
- Latin-like with light English roots

## Testing

The grammar has been tested with various Azalea code samples including:
- Variable declarations
- Function definitions
- Conditionals and loops
- UI components
- Backend servers

## References

- Language repository: https://github.com/xazalea/language
- Language documentation: https://github.com/xazalea/language/blob/main/docs/language_spec.md

