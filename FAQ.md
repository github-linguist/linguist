### How can I change the language of my repository?

The first language from the language statistics is sometimes shown next to the repository's name. If you believe the language statistics for your repository are incorrect, please see [*The language statistics in my repository are wrong*](FAQ.md#the-language-statistics-in-my-repository-are-wrong). If you believe we missed a language or one of its extensions, please consider [submitting a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-an-extension-to-a-language) if said extensions meet [the requirements](FAQ.md#what-are-the-requirements-to-associate-a-new-extension-to-a-language). If everything looks correct, but you'd still like another language to appear first, please consider using [Linguist overrides](https://github.com/github/linguist#overrides).

### The language detected for some files in my repository is incorrect.

Using [Linguist overrides](https://github.com/github/linguist#overrides), you can tell Linguist what's wrong.

It's also likely you can help us improve things. If you believe we don't support a language we should, and if that language is [widespread enough](FAQ.md#what-are-the-requirements-to-add-support-for-a-new-language), you can [send us a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-a-language). It is also possible to improve classification for existing languages by adding new [sample files](https://github.com/github/linguist/tree/master/samples) or by adding/improving [heuristic rules](https://github.com/github/linguist/blob/master/lib/linguist/heuristics.yml).


### I found a syntax highlighting error.

Linguist detects the language of a file, but the actual syntax highlighting is powered by a set of language grammars which are included in this project as a set of submodules as [listed here](https://github.com/github/linguist/blob/master/vendor/README.md).

If you experience an issue with the syntax highlighting on GitHub, please report the issue to the upstream grammar repository, not here. Grammars are updated automatically with every new release.

### When I click on a language in the statistics bar, no corresponding files are found in the search page.

This is a known bug that (unfortunately) doesn't fall under the purview of Linguist. Please [contact GitHub support](https://github.com/contact).

### No language is detected in my repository.

Only [programming and markup languages](FAQ.md#what-are-markup-or-programming-languages) are counted in the statistics. [Vendored](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml), [documentation](https://github.com/github/linguist/blob/master/lib/linguist/documentation.yml), and [generated](https://github.com/github/linguist/blob/master/lib/linguist/generated.rb) files are also excluded. Please also consider that Linguist runs as a low priority background job and it may therefore take some time for the languages to appear after you pushed to the repository.

Considering all this, if you still believe the repository should display a language, you can try to [run Linguist locally on your repository](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#getting-started) or you can open an issue.

### What are markup or programming languages?

In Linguist, each language has a type, which are documented in [`languages.yml`](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml).

### How does Linguist detect the language of a file?

Linguist relies on the [following strategies](https://github.com/github/linguist/blob/8bf9efa3702a1a43df85dc5cd72b63f3ff36871f/lib/linguist.rb#L61-L69), in order, and returns the language as soon as it found a perfect match (strategy with a single language returned).
1. Look for [Emacs and Vim modelines](https://github.com/github/linguist/blob/a878620a8ee6f45d89d8c6e1cdfbe49cb821ddfe/lib/linguist/strategy/modeline.rb).
0. Known filename. Some filenames are associated to specific languages (think [`Makefile`](https://github.com/github/linguist/blob/a878620a8ee6f45d89d8c6e1cdfbe49cb821ddfe/lib/linguist/languages.yml#L2532)).
0. Look for a shebang. A file with a [`#!/bin/bash` shebang](https://github.com/github/linguist/blob/a878620a8ee6f45d89d8c6e1cdfbe49cb821ddfe/lib/linguist/languages.yml#L4193) will be classified as Shell.
0. Known file extension. Languages have a set of extensions associated to them. There are, however, lots of conflicts with this strategy. The conflicting results (think C++, C and Objective-C for `.h`) are refined by the subsequent strategies.
0. An [XML-specific heuristic](https://github.com/github/linguist/blob/master/lib/linguist/strategy/xml.rb) matching on the root tag for XML documents.
0. A set of [heuristic rules](https://github.com/github/linguist/blob/master/lib/linguist/heuristics.rb). They usually rely on regular expressions over the content of files to try and identify the language (e.g., [`^[^#]+:-` for Prolog](https://github.com/github/linguist/blob/a878620a8ee6f45d89d8c6e1cdfbe49cb821ddfe/lib/linguist/heuristics.rb#L343)).
0. A naive Bayesian classifier trained on [sample files](https://github.com/github/linguist/tree/master/samples). Last strategy, lowest accuracy. The Bayesian classifier always takes a subset of languages as input; it is not meant to classify among all languages. The best match found by the classifier is returned.

### When will my pull request for Linguist take effect on github.com?

Changes to Linguist take effect on github.com with each new release, usually once a month.

### When will changes in a syntax highlighting grammar take effect on github.com?

Changes to any syntax highlighting grammar will take effect with the next release of Linguist, usually once a month. 

### I changed a syntax highlighting grammar. Do I need to open a pull request on Linguist for it to take effect on github.com?

No. Grammars are updated automatically with every new release.

### The language statistics in my repository are wrong.

The percentages in the statistic bar are calculated based on the total bytes of code for each [programming or markup language](FAQ.md#what-are-markup-or-programming-languages), after excluding [vendored](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml), [generated](https://github.com/github/linguist/blob/master/lib/linguist/generated.rb), and [documentation](https://github.com/github/linguist/blob/master/lib/linguist/documentation.yml) files. Considering this, if you believe the statistics are incorrect, it is likely that some files were incorrectly classified. Please read [*The language detected for some files in my repository is incorrect*](FAQ.md#the-language-detected-for-some-files-in-my-repository-is-incorrect) to fix it.

### How do I hide files in diffs?

To hide files in diffs, you can [mark them as generated](https://github.com/github/linguist#generated-code):
```
$ cat .gitattributes
files/to/mark/as/generated/* linguist-generated
```
If you believe Linguist should already recognize these files as generated, you can submit a pull request to improve [our identification of generated files](https://github.com/github/linguist/blob/master/lib/linguist/generated.rb).

### What are the requirements to associate a new extension to a language?

We prefer that each new file extension be in use in hundreds of repositories before supporting them in Linguist. In particular, we are wary of adding new languages for common extensions as they may conflict with other languages and cause misclassifications.

### What are the requirements to add support for a new language?

We prefer that each new file extension be in use in hundreds of repositories before supporting them in Linguist. In particular, we are wary of adding new languages with common extensions as they may conflict with other languages and cause misclassifications.

### How can I search for repositories that are using the language I want to add to Linguist?

As you may have noticed, GitHub doesn't offer a way to search for repositories containing files with a particular file extension. Instead, we recommend you use the [Code search](https://github.com/search?q=extension%3Ajava+NOT+randomstring154769). We will then use [the Harvester tool](https://github.com/Alhadis/Harvester) to deduce the number of repositories from the number of files. You may need to add `NOT randomstring` to your search query for GitHub to allow you to search file only by their extension. If several languages use that extension, you will need to add keywords to your search query to obtain a conservative estimation of the number of files for your particular language.

### Why aren't my files syntax highlighted?

There can be three reasons. Either the language for that file is not supported by Linguist, or Linguist doesn't have a grammar to highlight files from that language, or Linguist was unable to properly detect the language.
- If [the language is not supported by Linguist](FAQ.md#how-can-i-check-if-linguist-supports-a-given-language), and you believe it meets [the requirements for support](FAQ.md#what-are-the-requirements-to-add-support-for-a-new-language), please consider [submitting a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-a-language).
- To check if Linguist has a grammar for the language, you can check the [list of grammars](https://github.com/github/linguist/blob/master/vendor/README.md). If it doesn't and you know a Sublime Text, Atom, or TextMate grammar that would work, please consider [submitting a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#fixing-syntax-highlighting).
- If Linguist supports the language and it has a grammar, the lack of syntax highlighting is probably the result of a misclassification. Please read [*The language detected for some files in my repository is incorrect*](FAQ.md#the-language-detected-for-some-files-in-my-repository-is-incorrect) to fix it.

### How can I check if Linguist supports a given language?

The list of supported languages is listed in [`languages.yml`](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml), with the associated extensions, shebangs, and filenames.

### How do I disable syntax highlighting for a file?

You can disable syntax highlighting by telling Linguist the file is a Text file:
```
$ cat .gitattributes
path/to/the/file linguist-language=Text
```

### How are the language statistics computed?

The percentages in the statistic bar are calculated based on the total bytes of code for each [programming or markup language](FAQ.md#what-are-markup-or-programming-languages), after excluding [vendored](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml), [generated](https://github.com/github/linguist/blob/master/lib/linguist/generated.rb), and [documentation](https://github.com/github/linguist/blob/master/lib/linguist/documentation.yml) files.

### Why are some of my files not counted in language statistics?

Only files with a [markup or programming language](FAQ.md#what-are-markup-or-programming-languages) are counted in statistics. In addition, [generated](https://github.com/github/linguist/blob/master/lib/linguist/generated.rb), [documentation](https://github.com/github/linguist/blob/master/lib/linguist/documentation.yml), and [vendored](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml) files are excluded from statistics.

### What keywords can I use to highlight a code snippet in Markdown?

For each language in the [`languages.yml` file](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml), you can use as specifiers:
1. the language name;
0. any of the language `aliases`;
0. any of the language `interpreters`;
0. any of the file extensions, with or without a leading `.`.
White spaces must be replaced by dashes (e.g., `emacs-lisp` is one specifier for `Emacs Lisp`). Languages with a `tm_scope: none` entry don't have a grammar defined and won't be highlighted on github.com.

### How can I trigger an update of language detection in my repository?

Linguist runs as a low priority background job. It may therefore take a while, particularly during busy periods, for your language statistics bar to reflect your changes. To trigger a new analysis, you can push to your repository.

### How does Linguist highlight files?

Linguist detects the language of a file, but the actual syntax-highlighting is powered by a set of language grammars which are included in this project as a set of submodules as [listed here](https://github.com/github/linguist/blob/master/vendor/README.md).

### Can I define my own syntax highlighter for files in my repository?

If you wrote a syntax highlighter (Sublime Text, Atom, or TextMate grammar) for a language Linguist already support and it performs better than the syntax highlighter Linguist currently uses, please [submit a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#changing-the-source-of-a-syntax-highlighting-grammar)! If Linguist doesn't support said language, you will first need to add support for it (please see [the requirements](FAQ.md#what-are-the-requirements-to-add-support-for-a-new-language) first). GitHub doesn't currently offer a way to define custom syntax highlighter for unsupported languages.
