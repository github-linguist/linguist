# AGENTS.md

This file provides guidance for AI agents working with the Linguist repository.

## Project Overview

Linguist is a Ruby library used on GitHub.com to:
- Detect blob languages
- Ignore binary and vendored files
- Suppress generated files in diffs
- Generate language breakdown graphs

It's packaged as the `github-linguist` gem and includes CLI tools: `github-linguist` and `git-linguist`.

## Key Files and Directories

| Path | Purpose |
|------|---------|
| `lib/linguist/languages.yml` | Master list of all languages, extensions, filenames, and their properties |
| `lib/linguist/heuristics.yml` | Rules for disambiguating files with shared extensions |
| `lib/linguist/heuristics.rb` | Ruby code implementing heuristic logic |
| `lib/linguist/classifier.rb` | Centroid-based classifier for language detection |
| `samples/` | Sample code files for each language (used by classifier) |
| `grammars.yml` | Lists all TextMate grammars used for syntax highlighting |
| `vendor/` | Contains grammar submodules and cached license files for the grammars |
| `script/` | Utility scripts for maintenance tasks |

## Development Environment

Use GitHub Codespaces or the dev container for the easiest setup. The environment includes Ruby, Node.js, Docker, and all required dependencies.

To bootstrap locally:
```bash
script/bootstrap
```

To run Linguist from the repo:
```bash
bundle exec bin/github-linguist --breakdown
```

## Common Tasks

### Adding a Language Extension
1. Add extension to the language entry in `lib/linguist/languages.yml` (alphabetical order, case-sensitive; primary extension first)
2. Add sample files to `samples/<Language>/`
3. If extension is shared with another language, ensure there are at least two samples and add a heuristic in `lib/linguist/heuristics.yml`

### Adding a New Language
1. Add entry to `lib/linguist/languages.yml` (omit `language_id` initially)
2. Add grammar: `script/add-grammar <grammar-url>`
3. Add samples to `samples/<Language>/`
4. Generate ID: `script/update-ids`
5. If an extension is shared with another language, ensure there are at least two samples and add a heuristic in `lib/linguist/heuristics.yml`

### Replacing a Grammar
```bash
script/add-grammar --replace <GrammarName> <new-grammar-url>
```

### Important Scripts
- `script/bootstrap` - Install dependencies
- `script/add-grammar` - Add or replace TextMate grammars
- `script/update-ids` - Generate unique language IDs
- `script/cross-validation` - Test the classifier
- `script/cibuild` - CI build script

## Testing

Run tests with:
```bash
bundle exec rake test
```

Test the classifier:
```bash
bundle exec script/cross-validation --test
```

Always ensure tests pass before submitting changes. GitHub Actions runs tests on all PRs.

## Coding Conventions

- **Ruby style**: Follow existing code patterns in the repository
- **languages.yml**: Keep alphabetically sorted (case-sensitive, uppercase before lowercase) with the comment at the top. Use the comment at the top to determine the fields to add for a language.
- **Extensions in languages.yml**: Keep alphabetically sorted (case-sensitive, uppercase before lowercase); primary extension first
- **Samples**: Use real-world code examples, not "Hello World" and other common examples used in tutorials
- **Heuristics**: Write patterns to minimize false positives. Patterns must be linear, safe from ReDoS attacks, and RE2 compatible. All heuristics must have tests.
- **Don't refactor unrelated code.** Keep changes scoped to the task at hand.

## Generated and script maintained files — do not edit by hand

- `.gitmodules`
- `grammars.yml`
- all files in `vendor/`

These are all maintained by the `script/add-grammars` script.

## PR Requirements

All PRs must:
1. Use the provided PR template.
2. Link to GitHub search results showing in-the-wild usage, excluding forks, for each extension being added (minimum 2000 files for common extensions, 200 for once-per-repo files or extensions).
3. Link to the original source of any samples added. This must not be a link to the fork of Linguist used for the contribution.
4. State the license of any sample code added individually.
5. Pass all CI tests.

Do not open a PR if any of the above conditions are not met and notify the user why the PR has not been created.

## Usage Requirements for New Languages/Extensions

- At least 2000 files indexed on GitHub (or 200 for once-per-repo files like Makefile), excluding forks
- Reasonable distribution across unique `user/repo` combinations
- High-proportion users may be filtered out during assessment

## Grammar Requirements

Only add grammars with approved licenses (see `vendor/licenses/config.yml` for the list). TextMate-compatible grammars are used for syntax highlighting.

## Key Dependencies

- `charlock_holmes` - Character encoding detection (requires ICU)
- `rugged` - libgit2 bindings for Ruby
- Docker - Required for grammar operations

## Maintainers

- @Alhadis
- @lildude (GitHub staff)

PRs need approval from a GitHub staff member to be merged. Releases are performed by GitHub staff.
