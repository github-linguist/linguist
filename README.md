# Linguist

This library is used on GitHub.com to detect blob languages, ignore binary or vendored files, suppress generated files in diffs, and generate language breakdown graphs.

See [Troubleshooting](#troubleshooting) and [`CONTRIBUTING.md`](/CONTRIBUTING.md) before filing an issue or creating a pull request.

## Troubleshooting

### My repository is detected as the wrong language

![language stats bar](https://cloud.githubusercontent.com/assets/173/5562290/48e24654-8ddf-11e4-8fe7-735b0ce3a0d3.png)

The Language stats bar is built by aggregating the languages of each file in that repository. If it is reporting a language that you don't expect, click on the name of the language and to see a list of the files that are identified as that language. If there are files in your repository

consider moving the files into one of the

See [Overrides](#overrides)

## Overrides

Linguist supports custom overrides for language definitions and vendored paths. Please note that the overrides currently only affect the language statistics for a repository and not the syntax-highlighting of files.

Commit a `.gitattributes` file to your project and use standard git-style path matchers for the files you want to override to set `linguist-language` and `linguist-vendored`.

```
$ cat .gitattributes
*.rb linguist-language=Java
```

Checking code you didn't write, such as JavaScript libraries, into your git repo is a common practice, but this often inflates your project's language stats and may even cause your project to be labeled as another language.  By default, Linguist treats all of the paths defined in [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml) as vendored and therefore doesn't include them in the language statistics for a repository.

Use the `linguist-vendored` attribute to vendor or un-vendor paths.

```
$ cat .gitattributes
special-vendored-path/* linguist-vendored
jquery.js linguist-vendored=false
```
