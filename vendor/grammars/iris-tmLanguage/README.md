# iris-tmLanguage

This repository contains a permissively-licensed TextMate grammar for the IRIS programming language.

The grammar was copied from the IRIS project `vscode-iris` extension and is licensed under MIT here to allow GitHub Linguist vendoring.

To vendor this grammar into `github-linguist/linguist`, run:

```bash
# from a fork of github-linguist/linguist
script/add-grammar https://github.com/<your-account>/iris-tmLanguage
bundle exec rake test
```

See the upstream `github-linguist/linguist` CONTRIBUTING.md for full instructions.
