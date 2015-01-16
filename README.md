# Linguist

[issues]: https://github.com/github/linguist/issues
[new-issue]: https://github.com/github/linguist/issues/new

This library is used on GitHub.com to detect blob languages, ignore binary or vendored files, suppress generated files in diffs, and generate language breakdown graphs.

See [Troubleshooting](#troubleshooting) and [`CONTRIBUTING.md`](/CONTRIBUTING.md) before filing an issue or creating a pull request.

## Troubleshooting

### My repository is detected as the wrong language

![language stats bar](https://cloud.githubusercontent.com/assets/173/5562290/48e24654-8ddf-11e4-8fe7-735b0ce3a0d3.png)

The Language stats bar is built by aggregating the languages of each file in that repository. If it is reporting a language that you don't expect:

0. Click on the name of the language in the stats bar to see a list of the files that are identified as that language.
0. If you see files that you didn't write, consider moving the files into one of the [paths for vendored  code](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml), or use the [manual overrides](#overrides) feature to ignore them.
0. If the files are being misclassified, search for [open issues][issues] to see if anyone else has already reported the issue. Any information you an add, especially links to public repositories, is helpful.
0. If there are no reported issues of this misclassification, [open an issue][new-issue] and include a link to the repository or a sample of the code that is being misclassified.

## Overrides

Linguist supports custom overrides for language definitions and vendored paths. Please note that the overrides currently only affect the language statistics for a repository and not the syntax-highlighting of files.

Commit a `.gitattributes` file to your project and use standard git-style path matchers for the files you want to override to set `linguist-language` and `linguist-vendored`.

```
$ cat .gitattributes
*.rb linguist-language=Java
```

Checking code you didn't write, such as JavaScript libraries, into your git repo is a common practice, but this often inflates your project's language stats and may even cause your project to be labeled as another language. By default, Linguist treats all of the paths defined in [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml) as vendored and therefore doesn't include them in the language statistics for a repository.

Use the `linguist-vendored` attribute to vendor or un-vendor paths.

```
$ cat .gitattributes
special-vendored-path/* linguist-vendored
jquery.js linguist-vendored=false
```
