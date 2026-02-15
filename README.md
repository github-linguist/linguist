Aurum TextMate Grammar Contribution

This directory contains a ready-to-submit contribution package for GitHub Linguist.

Structure:
- aurum.tmLanguage.json  : TextMate JSON grammar (scope: source.aurum)
- examples/              : Example .au source files used as fixtures
- languages.yml.snippet  : YAML snippet to add to Linguist's languages.yml

Usage:
1. Fork https://github.com/github/linguist
2. Add `aurum.tmLanguage.json` into `grammars/` and example fixtures into `samples/` (follow Linguist conventions)
3. Update `lib/linguist/languages.yml` with the provided snippet.
4. Run Linguist's test suite (see CONTRIBUTING.md in Linguist) and open a PR.

Notes:
- This is an initial conservative grammar. After upstream review, iterations are expected.
