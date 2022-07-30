# How Linguist works

Linguist takes the list of languages it knows from [`languages.yml`](/lib/linguist/languages.yml) and uses a number of methods to try and determine the language used by each file, and the overall repository breakdown.

Linguist starts by going through all the files in a repository and excludes all files that it determines to be binary data, [vendored code](/docs/overrides.md#vendored-code), [generated code](/docs/overrides.md#generated-code), [documentation](/docs/overrides.md#documentation), or are defined as `data` (e.g. SQL) or `prose` (e.g. Markdown) languages, whilst taking into account any [overrides](/docs/overrides.md).

If an [explicit language override](/docs/overrides.md#using-gitattributes) has been used, that language is used for the matching files.
The language of each remaining file is then determined using the following strategies, in order, with each step either identifying the precise language or reducing the number of likely languages passed down to the next strategy:

- Vim or Emacs modeline,
- commonly used filename,
- shell shebang,
- file extension,
- XML header,
- man page section,
- heuristics,
- naïve Bayesian classification

The result of this analysis is used to produce the language stats bar which displays the languages percentages for the files in the repository.
The percentages are calculated based on the bytes of code for each language as reported by the [List Languages](https://docs.github.com/rest/reference/repos#list-repository-languages) API.

![language stats bar](https://user-images.githubusercontent.com/2346707/91533656-9768b300-e953-11ea-808d-994cd50e6273.png)

## How Linguist works on GitHub.com

When you push changes to a repository on GitHub.com, a low priority background job is enqueued to analyze your repository as explained above.
The results of this analysis are cached for the lifetime of your repository and are only updated when the repository is updated.
As this analysis is performed by a low priority background job, it can take a while, particularly during busy periods, for your language statistics bar to reflect your changes.

