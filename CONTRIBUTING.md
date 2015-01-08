## Contributing

The majority of contributions won't need to touch any Ruby code at all. The [master language list][languages] is just a YAML configuration file.

Almost all bug fixes or new language additions should come with some additional code samples. Just drop them under [`samples/`][samples] in the correct subdirectory and our test suite will automatically test them. In most cases you shouldn't need to add any new assertions.

### My code is detected as the wrong language

This can usually be solved either by adding a new filename or file name extension to the language's entry in [`languages.yml`][languages] or adding more [samples][samples] for your language to the repository to make Linguist's classifier smarter.

### Syntax highlighting looks wrong

Assuming your code is being detected as the right language (see above), in most cases this is due to a bug in the language grammar rather than a bug in Linguist. [`grammars.yml`][grammars] lists all the grammars we use for syntax highlighting on github.com. Find the one corresponding to your code's programming language and submit a bug report upstream. If you can, try to reproduce the highlighting problem in the text editor that the grammar is designed for (TextMate, Sublime Text, or Atom) and include that information in your bug report.

You can also try to fix the bug yourself and submit a Pull Request. [This piece from TextMate's documentation](http://manual.macromates.com/en/language_grammars) offers a good introduction on how to work with TextMate-compatible grammars. You can test grammars using [Lightshow](https://lightshow.githubapp.com).

Once the bug has been fixed upstream, please let us know and we'll pick it up for GitHub.

### I want to add support for the `X` programming language

Great! You'll need to:

0. Add an entry for your language to [`languages.yml`][languages].
0. Add a grammar for your language. Please only add grammars that have a license that permits redistribution.
    0. Add your grammar as a submodule: `git submodule add https://github.com/JaneSmith/MyGrammar vendor/grammars/MyGrammar`.
    0. Add your grammar to [`grammars.yml`][grammars] by running `script/convert-grammars --add vendor/grammars/MyGrammar`.
0. Add samples for your language to the [samples directory][samples].

In addition, if your new language defines an extension that's already listed in [`languages.yml`][languages] (such as `.foo`) then sometimes a few more steps will need to be taken:

0. Make sure that example `.foo` files are present in the [samples directory][samples] for each language that uses `.foo`. 
0. Test the performance of the Bayesian classifier with a relatively large number (1000s) of sample `.foo` files. (ping @arfon or @bkeepers to help with this) to ensure we're not misclassifying files.
0. If the Bayesian classifier does a bad job with the sample `.foo` files then a [heuristic](https://github.com/github/linguist/blob/master/lib/linguist/heuristics.rb) may need to be written to help.

Remember, the goal here is to try and avoid false positives!

We try only to add languages once they have some usage on GitHub, so please note in-the-wild usage examples in your pull request. In most cases we prefer that languages already be in use in hundreds of repositories before supporting them in Linguist.

[grammars]: /grammars.yml
[languages]: /lib/linguist/languages.yml
[samples]: /samples
