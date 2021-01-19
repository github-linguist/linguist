# Troubleshooting

## My repository is detected as the wrong language

If the language stats bar is reporting a language that you don't expect:

1. Click on the name of the language in the stats bar to see a list of the files that are identified as that language.
   Keep in mind this performs a search so the [code search restrictions][search-limits] may result in files identified in the language statistics not appearing in the search results.
   [Installing Linguist locally](#usage) and running it from the [command line](#command-line-usage) will give you accurate results.
1. If you see files that you didn't write in the search results, consider moving the files into one of the [paths for vendored code](/lib/linguist/vendor.yml), or use the [manual overrides](#overrides) feature to ignore them.
1. If the files are misclassified, search for [open issues][issues] to see if anyone else has already reported the issue.
   Any information you can add, especially links to public repositories, is helpful.
   You can also use the [manual overrides](#overrides) feature to correctly classify them in your repository.
1. If there are no reported issues of this misclassification, [open an issue][new-issue] and include a link to the repository or a sample of the code that is being misclassified.

[search-limits]: https://docs.github.com/github/searching-for-information-on-github/searching-code#considerations-for-code-search

Keep in mind that the repository language stats are only [updated when you push changes](#how-linguist-works-on-githubcom), and the results are cached for the lifetime of your repository.
If you have not made any changes to your repository in a while, you may find pushing another change will correct the stats.

## My repository isn't showing my language

Linguist does not consider [vendored code](#vendored-code), [generated code](#generated-code), [documentation](#documentation), or `data` (e.g. SQL) or `prose` (e.g. Markdown) languages (as defined by the `type` attribute in [`languages.yml`](/lib/linguist/languages.yml)) when calculating the repository language statistics.

If the language statistics bar is not showing your language at all, it could be for a few reasons:

1. Linguist doesn't know about your language.
1. The extension you have chosen is not associated with your language in [`languages.yml`](/lib/linguist/languages.yml).
1. All the files in your repository fall into one of the categories listed above that Linguist excludes by default.

If Linguist doesn't know about the language or the extension you're using, consider [contributing](CONTRIBUTING.md) to Linguist by opening a pull request to add support for your language or extension.
For everything else, you can use the [manual overrides](#overrides) feature to tell Linguist to include your files in the language statistics.

## There's a problem with the syntax highlighting of a file

Linguist detects the language of a file but the actual syntax-highlighting is powered by a set of language grammars which are included in this project as a set of submodules [as listed here](/vendor/README.md).

If you experience an issue with the syntax-highlighting on GitHub, **please report the issue to the upstream grammar repository, not here.**
Grammars are updated every time we build the Linguist gem so upstream bug fixes are automatically incorporated as they are fixed.

## I get an error when using Linguist on a directory that is not a Git repository

Linguist only works on Git repositories and individual files. Its primary use is on GitHub.com which uses bare
repositories and thus changes need to be committed as individual files don't show on the filesystem.

As a work around you could initialise a temporary Git repository in your directory as demonstrated in this
[script](https://gist.github.com/PuZZleDucK/a45fd1fac3758235ffed9fe0e8aab643).

Alternatively you can run Linguist on individual files, see [above](#single-file).
