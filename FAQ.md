<!-- Repeated links should go here -->
[`documentation.yml`]: https://github.com/github-linguist/linguist/blob/master/lib/linguist/documentation.yml
[`generated.rb`]: https://github.com/github-linguist/linguist/blob/master/lib/linguist/generated.rb
[`heuristics.yml`]: https://github.com/github-linguist/linguist/blob/master/lib/linguist/heuristics.yml
[`languages.yml`]: https://github.com/github-linguist/linguist/blob/master/lib/linguist/languages.yml
[`vendor.yml`]: https://github.com/github-linguist/linguist/blob/master/lib/linguist/vendor.yml
[grammars]: https://github.com/github-linguist/linguist/blob/master/vendor/README.md
[override]: https://github.com/github-linguist/linguist#overrides
[sample files]: https://github.com/github-linguist/linguist/tree/master/samples
[CONTRIBUTING.md]: https://github.com/github-linguist/linguist/tree/master/CONTRIBUTING.md


# Frequently Asked Questions

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Language detection](#language-detection)
  - [How does Linguist detect the language of a file?](#how-does-linguist-detect-the-language-of-a-file)
  - [How does Linguist work on GitHub.com?](#how-does-linguist-work-on-githubcom)
  - [How is a repository's language determined?](#how-is-a-repositorys-language-determined)
  - [How can I change the language of my repository?](#how-can-i-change-the-language-of-my-repository)
  - [Why is language detected for some files in my repository is incorrect?](#why-is-language-detected-for-some-files-in-my-repository-is-incorrect)
  - [Why are no files found in the search page when I click on a language in the statistics bar?](#why-are-no-files-found-in-the-search-page-when-i-click-on-a-language-in-the-statistics-bar)
  - [Why does the language bar show some languages, but never others?](#why-does-the-language-bar-show-some-languages-but-never-others)
  - [Why hasn't the language bar changed after I pushed changes to my repository?](#why-hasnt-the-language-bar-changed-after-i-pushed-changes-to-my-repository)
  - [What are markup or programming languages?](#what-are-markup-or-programming-languages)
  - [The language statistics in my repository are wrong](#the-language-statistics-in-my-repository-are-wrong)
  - [How do I hide files in diffs?](#how-do-i-hide-files-in-diffs)
  - [Why are some of my files not counted in the language statistics?](#why-are-some-of-my-files-not-counted-in-the-language-statistics)
  - [How can I trigger an update of the language detection in my repository?](#how-can-i-trigger-an-update-of-the-language-detection-in-my-repository)
- [Syntax Highlighting](#syntax-highlighting)
  - [Why aren't my files syntax highlighted?](#why-arent-my-files-syntax-highlighted)
  - [How do I disable syntax highlighting for a file?](#how-do-i-disable-syntax-highlighting-for-a-file)
  - [What keywords can I use to highlight a code snippet in Markdown?](#what-keywords-can-i-use-to-highlight-a-code-snippet-in-markdown)
  - [I found a syntax highlighting error](#i-found-a-syntax-highlighting-error)
  - [I've updated a syntax highlighting grammar. What should I do?](#ive-updated-a-syntax-highlighting-grammar-what-should-i-do)
  - [Can I define my own syntax highlighter for files in my repository?](#can-i-define-my-own-syntax-highlighter-for-files-in-my-repository)
- [Contributing to Linguist](#contributing-to-linguist)
  - [How can I check if Linguist supports a given language?](#how-can-i-check-if-linguist-supports-a-given-language)
  - [How do I add a new language?](#how-do-i-add-a-new-language)
  - [How do I add an extension to a language?](#how-do-i-add-an-extension-to-a-language)
  - [How do I fix a syntax highlighting problem?](#how-do-i-fix-a-syntax-highlighting-problem)
  - [How do I change the source of a syntax highlighting grammar?](#how-do-i-change-the-source-of-a-syntax-highlighting-grammar)
  - [How do I pick a color for a language?](#how-do-i-pick-a-color-for-a-language)
  - [What are the dependencies for contributing to Linguist?](#what-are-the-dependencies-for-contributing-to-linguist)
  - [What are the requirements to add support for a new language or associate a new extension with a language?](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language)
  - [How can I search for repositories that are using the language or extension I want to add to Linguist?](#how-can-i-search-for-repositories-that-are-using-the-language-or-extension-i-want-to-add-to-linguist)
  - [When will my pull request for Linguist take effect on GitHub.com?](#when-will-my-pull-request-for-linguist-take-effect-on-githubcom)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

---

## Language detection

### How does Linguist detect the language of a file?

See [*How Linguist Works*](https://github.com/github-linguist/linguist/blob/master/docs/how-linguist-works.md).


### How does Linguist work on GitHub.com?

See [the README](https://github.com/github-linguist/linguist/blob/master/docs/how-linguist-works.md#how-linguist-works-on-githubcom).


### How is a repository's language determined?

A common misconception is that GitHub reports a repository language.
This is not the case.
Instead, GitHub uses a concept of *"this repository contains these languages in X, Y and Z proportions"*, and these proportions are determined by Linguist based on the bytes of code found within the repository.
The "repository language" you see is the most prominent language detected and this is what is is shown next to the repository's name on some pages.


### How can I change the language of my repository?

If you believe the language statistics for your repository are incorrect, please see [*The language statistics in my repository are wrong*](#the-language-statistics-in-my-repository-are-wrong).
If you believe we missed a language or one of its extensions, please consider [submitting a pull request](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#adding-an-extension-to-a-language) if said extensions meet [the requirements](#what-are-the-requirements-to-associate-a-new-extension-to-a-language).
If everything looks correct, but you'd still like another language to appear first, please consider using an [override][].


### Why is language detected for some files in my repository is incorrect?

You can force Linguist to determine the correct language using an [override][].

It's also likely you can help us improve things.
If you believe we don't support a language we should, and if that language is [widespread enough](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language), you can [send us a pull request](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#adding-a-language).
It is also possible to improve classification for existing languages by adding new [sample files][] or by adding/improving the [heuristic rules][`heuristics.yml`].


### Why are no files found in the search page when I click on a language in the statistics bar?

This is commonly caused by the size of the files for that specific language or the language being a very new addition to Linguist.
GitHub does not index files larger than 350 KiB, however they still count towards the language statistics.
Other [code search restrictions](https://docs.github.com/en/search-github/github-code-search/about-github-code-search#limitations) could also apply.

GitHub's search uses its own library for detecting languages which is based on Linguist, however it can lag behind Linguist so search will not know about a language that Linguist has detected.
This will be addressed when the library is next synchronized with Linguist.

If all of the files you are expecting to see meet all the search considerations and still aren't appearing in the search results, please [contact GitHub support](https://github.com/contact).


### Why does the language bar show some languages, but never others?

Only [programming and markup languages](#what-are-markup-or-programming-languages) are counted in the language statistics.
Files that are considered vendored, documentation, data languages, or generated will be excluded by default.

For example, if all your code occurs in a directory called `vendor` it will be excluded as `vendor` in the path is considered vendored.
Similarly, if all your code is in a directory called `examples`, it'll be excluded as files under `examples` are considered documentation.
You can see a list of all the paths and criteria listed in:

- [`vendor.yml`][],
- [`documentation.yml`][],
- [`generated.rb`][].


### Why hasn't the language bar changed after I pushed changes to my repository?

Linguist runs as a low priority background job and it may therefore take some time for the languages to appear after you have pushed to the repository.

Considering this, if you still believe the repository should display a language, you can try to [run Linguist locally on your repository](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#getting-started) or you can open an issue.


### What are markup or programming languages?

Linguist distinguishes between 4 different language types
1. **Programming:**
Executed or interpreted source code (e.g., C++, JavaScript, Perl, Assembly, etc)
2. **Markup:**
Code defining a document's presentation or formatting.
This can include "true" markup languages (e.g., HTML), templating languages (e.g., Blade, Handlebars), or programming languages mostly used as page-description languages (e.g., PostScript, Roff, TeX).
3. **Prose:**
Similar to markup, except the content is composed mostly of human-readable content, and can usually be consume without additional processing.
Most "lightweight markup" languages fall into this category (e.g., Markdown, AsciiDoc).
4. **Data:**
If it's plain-text which doesn't fit into any of the above categories, it probably comes under `Data`.

Of these types, only markup and programming count towards the language statistics, though you can override this behaviour with the [detectable override](https://github.com/github-linguist/linguist#detectable).


### The language statistics in my repository are wrong

The percentages in the statistics bar are calculated based on the total **bytes of code** for each [programming or markup language](#what-are-markup-or-programming-languages), after excluding [vendored][`vendor.yml`], [generated][`generated.rb`], and [documentation][`documentation.yml`] files.
Considering this, if you believe the statistics are incorrect, it is likely that some files were incorrectly classified.
Please read [*The language detected for some files in my repository is incorrect*](#the-language-detected-for-some-files-in-my-repository-is-incorrect) to fix it.


### How do I hide files in diffs?

To hide files in diffs, you can [mark them as generated](https://github.com/github-linguist/linguist#generated-code) with an override in the `.gitattributes` file:

```gitattributes
files/to/mark/as/generated/* linguist-generated
```

If you believe Linguist should already recognize these files as generated, you can submit a pull request to improve [our identification of generated files][`generated.rb`].


### Why are some of my files not counted in the language statistics?

Only files with a [markup or programming language](#what-are-markup-or-programming-languages) are counted in statistics.
In addition, [generated][`generated.rb`], [documentation][`documentation.yml`], and [vendored][`vendor.yml`] files are excluded from statistics.


### How can I trigger an update of the language detection in my repository?

You can trigger a new analysis of your repository by pushing a change to your repository.
This will enqueue a low priority background job that will analyze your repository.
Keep in mind that it may take a while, particularly during busy periods, for your language statistics bar to reflect your changes.

---

## Syntax Highlighting

### Why aren't my files syntax highlighted?

There are three primary reasons why a file may not be highlighted:

- the language for that file is not supported by Linguist, or
- Linguist doesn't have a grammar to highlight files for that language or
- Linguist was unable to properly detect the language.

If [the language is not supported by Linguist](#how-can-i-check-if-linguist-supports-a-given-language), and you believe it meets [the requirements for support](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language), please consider [submitting a pull request](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#adding-a-language).

You can see if Linguist has a grammar for the language in the [list of grammars][grammars].
If it doesn't and you know a Sublime Text 2, VS Code, Atom or TextMate grammar that would work, please consider [submitting a pull request](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#fixing-syntax-highlighting).

If Linguist supports the language and it has a grammar, the lack of syntax highlighting is probably the result of a misclassification.
Please read [*The language detected for some files in my repository is incorrect*](#the-language-detected-for-some-files-in-my-repository-is-incorrect) to fix it.

If a file was previously misclassified and you have implemented an override, you will need to push a change for the affected file too to invalidate the cached syntax highlighting.


### How do I disable syntax highlighting for a file?

You can't.
However, you can use `.gitattributes` to flag a file as ordinary text instead of source code:

```gitattributes
path/to/unhighlighted-file/* linguist-language=Text
```

You will need to push a change for the affected file too to invalidate the cached syntax highlighting.
Be aware that doing so will affect your repository's language statistics.


### What keywords can I use to highlight a code snippet in Markdown?

For each language in [`languages.yml`][], you can use as specifiers:
1. the language name;
1. any of the language `aliases`;
1. any of the language `interpreters`;
1. any of the file extensions, with or without a leading `.`.
   Whitespace must be replaced by dashes, for example, `emacs-lisp` is one specifier for `Emacs Lisp`.
   Languages with a `tm_scope: none` entry don't have a grammar defined and won't be highlighted on GitHub.


### I found a syntax highlighting error

Linguist detects the language of a file, but the actual syntax highlighting is powered by a set of language grammars which are included in this project as a set of submodules as [listed here][grammars].

Please report it to the upstream grammar repository. You can locate the defective repository in [the list of grammars](https://github.com/github-linguist/linguist/tree/master/vendor) which Linguist uses to highlight source code.

Once the error has been fixed, it'll disappear with the next release of Linguist.


### I've updated a syntax highlighting grammar. What should I do?

Nothing.
All grammars are updated automatically when a new release of Linguist is prepared.

Changes to upstream grammars (as well as Linguist itself) won't become visible on GitHub.com until the next release of Linguist.


### Can I define my own syntax highlighter for files in my repository?

GitHub doesn't currently offer a way to define a custom grammar for unsupported languages.
All support needs to be provided through Linguist.

If you have a Sublime Text 2, VS Code, Atom, or TextMate grammar for a language Linguist already supports and it performs better than the grammar Linguist currently uses, please [submit a pull request](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#changing-the-source-of-a-syntax-highlighting-grammar)!

If Linguist doesn't currently support your language, you will need to add support for it, provided it meets [the requirements](#what-are-the-requirements-to-add-support-for-a-new-language-or-associate-a-new-extension-with-a-language) for inclusion.

---

## Contributing to Linguist

### How can I check if Linguist supports a given language?

The list of supported languages is listed in [`languages.yml`][], with the associated extensions, shebangs and filenames.


### How do I add a new language?

Please see [*Adding a language*](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#adding-a-language).


### How do I add an extension to a language?

Please see [*Adding an extension to a language*](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#adding-an-extension-to-a-language).


### How do I fix a syntax highlighting problem?

Please see [*Fixing syntax highlighting*](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#fixing-syntax-highlighting).


### How do I change the source of a syntax highlighting grammar?

Please see [*Changing the source of a syntax highlighting grammar*](https://github.com/github-linguist/linguist/blob/master/CONTRIBUTING.md#changing-the-source-of-a-syntax-highlighting-grammar).


### How do I pick a color for a language?

When selecting the color for a new language, we recommend you start with the language's official branding guidelines and pick a color users will associate with the language.
If there are no official branding guidelines, a good place to start is the predominant color in the language's logo.

If you want to change the color of an existing language, you will need to obtain agreement from the wider community of that language in an official publicly accessible location for that language.
Do not use Linguist's issues, pull requests or discussions for this as these are for the Linguist community, not the language in question.


### What are the dependencies for contributing to Linguist?

Simple additions like adding a new extension or improving the heuristics don't require Ruby or any of the other listed [dependencies](https://github.com/github-linguist/linguist/tree/master/CONTRIBUTING.md#dependencies), and can be performed directly on GitHub.
More complicated changes like adding a new language or changing the source of a syntax highlighting grammar will require all the [dependencies](https://github.com/github-linguist/linguist/tree/master/CONTRIBUTING.md#dependencies) to be installed as many of the scripts rely on these dependencies.


### What are the requirements to add support for a new language or associate a new extension with a language?

We try only to add languages once they have wide usage on GitHub.
In most cases we prefer that each new file extension be in use in at least 200 unique `:user/:repo` repositories before supporting them in Linguist (but see [#5756][] for a temporary change in the criteria).
In particular, we are wary of adding new languages for common extensions as they may conflict with other languages and cause misclassifications.


### How can I search for repositories that are using the language or extension I want to add to Linguist?

GitHub doesn't offer a way to search for repositories containing files with a particular file extension.
Instead, we recommend you use the [code search](https://github.com/search?q=extension%3Ajava+NOT+randomstring154769) with the query `NOT is:fork path:*.ext`.

If several languages use that extension, you will need to add keywords or [refine your search query](https://docs.github.com/en/search-github/github-code-search/understanding-github-code-search-syntax) to obtain a conservative and more accurate estimation of the number of files for your particular language.


### When will my pull request for Linguist take effect on GitHub.com?

Please see [*My Linguist PR has been merged but GitHub doesn't reflect my changes*](https://github.com/github-linguist/linguist/blob/master/docs/troubleshooting.md#my-linguist-pr-has-been-merged-but-github-doesnt-reflect-my-changes).
