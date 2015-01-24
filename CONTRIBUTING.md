## Contributing

The majority of contributions won't need to touch any Ruby code at all. The [master language list][languages] is just a YAML configuration file.

Almost all bug fixes or new language additions should come with some additional code samples. Just drop them under [`samples/`][samples] in the correct subdirectory and our test suite will automatically test them. In most cases you shouldn't need to add any new assertions.

### My code is detected as the wrong language

This can usually be solved either by adding a new filename or file name extension to the language's entry in [`languages.yml`][languages] or adding more [samples][samples] for your language to the repository to make Linguist's classifier smarter.

### Syntax highlighting looks wrong

Assuming your code is being detected as the right language (see above), in most cases this is due to a bug in the language grammar rather than a bug in Linguist. [`grammars.yml`][grammars] lists all the grammars we use for syntax highlighting on github.com. Find the one corresponding to your code's programming language and submit a bug report upstream.

You can also try to fix the bug yourself and submit a Pull Request. [This piece from TextMate's documentation](http://manual.macromates.com/en/language_grammars) offers a good introduction on how to work with TextMate-compatible grammars.

Once the bug has been fixed upstream, please let us know and we'll pick it up for GitHub.

### I want to add support for the `X` programming language

Great! You'll need to:

0. Add an entry for your language to [`languages.yml`][languages].
0. Add a grammar for your language to [`grammars.yml`][grammars] by running `script/download-grammars --add URL`. Please only add grammars that have a license that permits redistribution.
0. Add samples for your language to the [samples directory][samples].

We try only to add languages once they have some usage on GitHub, so please note in-the-wild usage examples in your pull request. In most cases we prefer that languages already be in use in hundreds of repositories before supporting them in Linguist.

[grammars]: /grammars.yml
[languages]: /lib/linguist/languages.yml
[samples]: /samples
