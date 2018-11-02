<!-- Repeated links should go here -->
[`documentation.yml`]: https://github.com/github/linguist/blob/master/lib/linguist/documentation.yml
[`generated.rb`]: https://github.com/github/linguist/blob/master/lib/linguist/generated.rb
[`heuristics.yml`]: https://github.com/github/linguist/blob/master/lib/linguist/heuristics.yml
[`languages.yml`]: https://github.com/github/linguist/blob/master/lib/linguist/languages.yml
[`vendor.yml`]: https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml
[grammars]: https://github.com/github/linguist/blob/master/vendor/README.md
[modelines]: https://github.com/github/linguist/blob/master/README.md#using-emacs-or-vim-modelines
[override]: https://github.com/github/linguist#overrides
[sample files]: https://github.com/github/linguist/tree/master/samples


# Frequently Asked Questions

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Language Detection](#language-detection)
  - [How does Linguist detect the language of a file?](#how-does-linguist-detect-the-language-of-a-file)
  - [How does Linguist work on GitHub.com?](#how-does-linguist-work-on-githubcom)
  - [How can I change the language of my repository?](#how-can-i-change-the-language-of-my-repository)
  - [The language detected for some files in my repository is incorrect.](#the-language-detected-for-some-files-in-my-repository-is-incorrect)
  - [When I click on a language in the statistics bar, no corresponding files are found in the search page.](#when-i-click-on-a-language-in-the-statistics-bar-no-corresponding-files-are-found-in-the-search-page)
  - [No language is detected in my repository.](#no-language-is-detected-in-my-repository)
  - [What are markup or programming languages?](#what-are-markup-or-programming-languages)
  - [How are the language statistics computed?](#how-are-the-language-statistics-computed)
  - [The language statistics in my repository are wrong.](#the-language-statistics-in-my-repository-are-wrong)
  - [How do I hide files in diffs?](#how-do-i-hide-files-in-diffs)
  - [Why are some of my files not counted in the language statistics?](#why-are-some-of-my-files-not-counted-in-the-language-statistics)
  - [How can I trigger an update of the language detection in my repository?](#how-can-i-trigger-an-update-of-the-language-detection-in-my-repository)
- [Syntax Highlighting](#syntax-highlighting)
  - [Why aren't my files syntax highlighted?](#why-arent-my-files-syntax-highlighted)
  - [How do I disable syntax highlighting for a file?](#how-do-i-disable-syntax-highlighting-for-a-file)
  - [What keywords can I use to highlight a code snippet in Markdown?](#what-keywords-can-i-use-to-highlight-a-code-snippet-in-markdown)
  - [I found a syntax highlighting error.](#i-found-a-syntax-highlighting-error)
  - [When will changes in a syntax highlighting grammar take effect on GitHub.com?](#when-will-changes-in-a-syntax-highlighting-grammar-take-effect-on-githubcom)
  - [I changed a syntax highlighting grammar. Do I need to open a pull request on Linguist for it to take effect on GitHub.com?](#i-changed-a-syntax-highlighting-grammar-do-i-need-to-open-a-pull-request-on-linguist-for-it-to-take-effect-on-githubcom)
  - [How does Linguist highlight files?](#how-does-linguist-highlight-files)
  - [Can I define my own syntax highlighter for files in my repository?](#can-i-define-my-own-syntax-highlighter-for-files-in-my-repository)
- [Contributing to Linguist](#contributing-to-linguist)
  - [How can I check if Linguist supports a given language?](#how-can-i-check-if-linguist-supports-a-given-language)
  - [How do I add a new language?](#how-do-i-add-a-new-language)
  - [How do I add an extension to a language?](#how-do-i-add-an-extension-to-a-language)
  - [What are the requirements to add support for a new language or associate a new extension with a language?](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language)
  - [How can I search for repositories that are using the language or extension I want to add to Linguist?](#how-can-i-search-for-repositories-that-are-using-the-language-or-extension-i-want-to-add-to-linguist)
  - [When will my pull request for Linguist take effect on GitHub.com?](#when-will-my-pull-request-for-linguist-take-effect-on-githubcom)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

---

## Language detection

### How does Linguist detect the language of a file?

Linguist takes the list of languages it knows from [`languages.yml`][] and uses the [following strategies](https://github.com/github/linguist/blob/8bf9efa3702a1a43df85dc5cd72b63f3ff36871f/lib/linguist.rb#L61-L69), in order, with each step either identifying the precise language or reducing the number of likely languages passed down to the next strategy, until a single language is identified:

1. Use the language defined by a [Vim or Emacs modeline][modelines].
2. Use the language associated with a commonly used filename, for example [`Makefile`](https://github.com/github/linguist/blob/a878620a8ee6f45d89d8c6e1cdfbe49cb821ddfe/lib/linguist/languages.yml#L2528-L2532).
3. Use the language associated with a shebang in the file, for a example a file with a [`#!/bin/bash` shebang](https://github.com/github/linguist/blob/a878620a8ee6f45d89d8c6e1cdfbe49cb821ddfe/lib/linguist/languages.yml#L4193) will be classified as Shell.
4. Use the language associated with the file extension, for example `.php`. Languages that share a common extension, for example C, C++ and Objective-C use `.h`, are further refined by subsequent strategies.
5. Use XML if an [XML root tag is found](https://github.com/github/linguist/blob/master/lib/linguist/strategy/xml.rb).
6. Use the language determined by a set of regexp-based [heuristic rules][`heuristics.yml`].
7. Use the best matched language returned by a naïve Bayesian classifier trained on [sample files][]. This is the last strategy with the lowest accuracy. The classifier always takes a subset of languages as input; it is not meant to classify all languages.


### How does Linguist work on GitHub.com?

When you push changes to a repository on GitHub.com, a low priority background job is enqueued to analyze the files in your repository as explained in [*How does Linguist detect the language of a file?*](#how-does-linguist-detect-the-language-of-a-file). The results of this analysis are cached for the lifetime of your repository and are only updated when the repository is updated.

As this analysis is performed by a low priority background job, it can take a while, particularly during busy periods, for your language statistics bar to reflect your changes.


### How can I change the language of my repository?

A common misconception is that GitHub reports a repository language. This is not the case. Instead, GitHub uses a concept of _"this repository contains these languages in X, Y and Z proportions"_, and these proportions are determined by Linguist based on the bytes of code found within the repository. The "repository language" you see is the most prominent language detected and this is what is is shown next to the repository's name on some pages.

If you believe the language statistics for your repository are incorrect, please see [*The language statistics in my repository are wrong*](#the-language-statistics-in-my-repository-are-wrong). If you believe we missed a language or one of its extensions, please consider [submitting a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-an-extension-to-a-language) if said extensions meet [the requirements](#what-are-the-requirements-to-associate-a-new-extension-to-a-language). If everything looks correct, but you'd still like another language to appear first, please consider using an [override][].


### The language detected for some files in my repository is incorrect.

You can force Linguist to determine the correct language using an [override][].

It's also likely you can help us improve things. If you believe we don't support a language we should, and if that language is [widespread enough](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language), you can [send us a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-a-language). It is also possible to improve classification for existing languages by adding new [sample files][] or by adding/improving the [heuristic rules][`heuristics.yml`].


### When I click on a language in the statistics bar, no corresponding files are found in the search page.

This is commonly caused by the size of the files for that specific language. GitHub does not index files larger than 384KB, however they still count towards the language statistics. Other [code search restrictions](https://help.github.com/articles/searching-code/#considerations-for-code-search) could also apply.

If all of the files you are expecting to see meet all the search considerations and still aren't appearing in the search results, please [contact GitHub support](https://github.com/contact).


### No language is detected in my repository.

Only [programming and markup languages](#what-are-markup-or-programming-languages) are counted in the language statistics. Files that are considered vendored, documentation, data languages, or generated will be excluded by default.

For example, if all your code occurs in a directory called `vendor` it will be excluded as `vendor` in the path is considered vendored. Similarly, if all your code is in a directory called `examples`, it'll be excluded as files under `examples` are considered documentation. You can see a list of all the paths and criteria listed in:

- [`vendor.yml`][],
- [`documentation.yml`][],
- [`generated.rb`][].

Please also consider that Linguist runs as a low priority background job and it may therefore take some time for the languages to appear after you have pushed to the repository.

Considering all this, if you still believe the repository should display a language, you can try to [run Linguist locally on your repository](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#getting-started) or you can open an issue.


### What are markup or programming languages?

When a language is added to Linguist, one of four types is associated with it in [`languages.yml`](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml): `markup`, `programming`, `prose` or `data`.

- Markup languages are those that are designed for the processing, definition and presentation of text, like HTML and CSS.
- Programming languages are those that are designed to create a program, like C, Ruby and Bash.
- Prose languages are those that are used for writing and lightly formatting of text, like Markdown and AsciiDoc.
- Data languages are those that are commonly associated with storing of data, like XML and JSON.

Of these types, only markup and programming count towards the language statistics, though you can override this behaviour with the [detectable override](https://github.com/github/linguist#detectable).


### How are the language statistics computed?

The percentages in the statistics bar are calculated based on the total **bytes of code** for each [programming or markup language](#what-are-markup-or-programming-languages), after excluding [vendored][`vendor.yml`], [generated][`generated.rb`], and [documentation][`documentation.yml`] files.


### The language statistics in my repository are wrong.

The percentages in the statistics bar are calculated based on the total **bytes of code** for each [programming or markup language](#what-are-markup-or-programming-languages), after excluding [vendored][`vendor.yml`], [generated][`generated.rb`], and [documentation][`documentation.yml`] files. Considering this, if you believe the statistics are incorrect, it is likely that some files were incorrectly classified. Please read [*The language detected for some files in my repository is incorrect*](#the-language-detected-for-some-files-in-my-repository-is-incorrect) to fix it.


### How do I hide files in diffs?

To hide files in diffs, you can [mark them as generated](https://github.com/github/linguist#generated-code):

```
$ cat .gitattributes
files/to/mark/as/generated/* linguist-generated
```

If you believe Linguist should already recognize these files as generated, you can submit a pull request to improve [our identification of generated files][`generated.rb`].


### Why are some of my files not counted in the language statistics?

Only files with a [markup or programming language](#what-are-markup-or-programming-languages) are counted in statistics. In addition, [generated][`generated.rb`], [documentation][`documentation.yml`], and [vendored][`vendor.yml`] files are excluded from statistics.


### How can I trigger an update of the language detection in my repository?

You can trigger a new analysis of your repository by pushing a change to your repository. This will enqueue a low priority background job that will analyze your repository. Keep in mind that it may take a while, particularly during busy periods, for your language statistics bar to reflect your changes.

---

## Syntax Highlighting

### Why aren't my files syntax highlighted?

There are three primary reasons why a file may not be highlighted:

- the language for that file is not supported by Linguist, or
- Linguist doesn't have a grammar to highlight files for that language or
- Linguist was unable to properly detect the language.

If [the language is not supported by Linguist](#how-can-i-check-if-linguist-supports-a-given-language), and you believe it meets [the requirements for support](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language), please consider [submitting a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-a-language).

You can see if Linguist has a grammar for the language in the [list of grammars][grammars]. If it doesn't and you know a Sublime Text, Atom or TextMate grammar that would work, please consider [submitting a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#fixing-syntax-highlighting).

If Linguist supports the language and it has a grammar, the lack of syntax highlighting is probably the result of a misclassification. Please read [*The language detected for some files in my repository is incorrect*](#the-language-detected-for-some-files-in-my-repository-is-incorrect) to fix it.

If a file was previously misclassified and you have implemented an override, you will need to push a change for the affected file too to invalidate the cached syntax highlighting.


### How do I disable syntax highlighting for a file?

You can disable syntax highlighting by telling Linguist the file is a Text file:

```
$ cat .gitattributes
path/to/the/file linguist-language=Text
```

You will need to push a change for the affected file too to invalidate the cached syntax highlighting.


### What keywords can I use to highlight a code snippet in Markdown?

For each language in [`languages.yml`][], you can use as specifiers:
1. the language name;
1. any of the language `aliases`;
1. any of the language `interpreters`;
1. any of the file extensions, with or without a leading `.`.
White spaces must be replaced by dashes, for example, `emacs-lisp` is one specifier for `Emacs Lisp`. Languages with a `tm_scope: none` entry don't have a grammar defined and won't be highlighted on GitHub.com.


### I found a syntax highlighting error.

Linguist detects the language of a file, but the actual syntax highlighting is powered by a set of language grammars which are included in this project as a set of submodules as [listed here][grammars].

If you experience an issue with the syntax highlighting on GitHub.com, please report the issue to the upstream grammar repository, not here. Grammars are updated automatically with every new release.


### When will changes in a syntax highlighting grammar take effect on GitHub.com?

Changes to any syntax highlighting grammar will take effect with the next release of Linguist, usually once a month.


### I changed a syntax highlighting grammar. Do I need to open a pull request on Linguist for it to take effect on GitHub.com?

No. Grammars are updated automatically with every new release.


### How does Linguist highlight files?

Linguist detects the language of a file, but the actual syntax-highlighting is powered by a set of language grammars which are included in this project as a set of submodules as [listed here][grammars].


### Can I define my own syntax highlighter for files in my repository?

GitHub doesn't currently offer a way to define a custom grammar for unsupported languages within a specific repository. All support needs to be provided through Linguist.

If you have a Sublime Text, Atom, or TextMate grammar for a language Linguist already supports and it performs better than the grammar Linguist currently uses, please [submit a pull request](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#changing-the-source-of-a-syntax-highlighting-grammar)!

If Linguist doesn't currently support your language, you will need to add support for it, provided it meets [the requirements](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language) for inclusion.

---

## Contributing to Linguist

### How can I check if Linguist supports a given language?

The list of supported languages is listed in [`languages.yml`][], with the associated extensions, shebangs and filenames.


### How do I add a new language?

Please see [*Adding a language*](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-a-language).


### How do I add an extension to a language?

Please see [*Adding an extension to a language*](https://github.com/github/linguist/blob/master/CONTRIBUTING.md#adding-an-extension-to-a-language).


### What are the requirements to add support for a new language or associate a new extension with a language?

We prefer that each new file extension be in use in hundreds of repositories before supporting them in Linguist. In particular, we are wary of adding new languages for common extensions as they may conflict with other languages and cause misclassifications.


### How can I search for repositories that are using the language or extension I want to add to Linguist?

GitHub doesn't offer a way to search for repositories containing files with a particular file extension. Instead, we recommend you use the [Code search](https://github.com/search?q=extension%3Ajava+NOT+randomstring154769). We will then use [the Harvester tool](https://github.com/Alhadis/Harvester) to deduce the number of repositories from the number of files. You may need to add `NOT randomstring` to your search query for GitHub to allow you to search files only by their extension. If several languages use that extension, you will need to add keywords to your search query to obtain a conservative estimation of the number of files for your particular language.


### When will my pull request for Linguist take effect on GitHub.com?

Changes to Linguist take effect on GitHub.com with each new release, usually once a month.
