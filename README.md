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

Linguist supports a number of different custom overrides strategies for language definitions and vendored paths.

### Using gitattributes

Add a `.gitattributes` file to your project and use standard git-style path matchers for the files you want to override to set `linguist-documentation`, `linguist-language`, and `linguist-vendored`. `.gitattributes` will be used to determine language statistics, but will not be used to syntax highlight files. To manually set syntax highlighting, use [Vim or Emacs modelines](#using-emacs-and-vim-modelines).

```
$ cat .gitattributes
*.rb linguist-language=Java
```

Checking code you didn't write, such as JavaScript libraries, into your git repo is a common practice, but this often inflates your project's language stats and may even cause your project to be labeled as another language. By default, Linguist treats all of the paths defined in [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml) as vendored and therefore doesn't include them in the language statistics for a repository. Vendored files are also hidden by default in diffs on github.com.

Use the `linguist-vendored` attribute to vendor or un-vendor paths. Please note, overriding the vendored (or un-vendored) status of a file only affects the language statistics for the repository and not the behavior in diffs on github.com.

```
$ cat .gitattributes
special-vendored-path/* linguist-vendored
jquery.js linguist-vendored=false
```

Similar to vendored files, Linguist excludes documentation files from your project's language stats. (Unlike vendored files, documentation files are displayed in diffs on github.com.) [lib/linguist/documentation.yml](lib/linguist/documentation.yml) lists common documentation paths and excludes them from the language statistics for your repository.

Use the `linguist-documentation` attribute to mark or unmark paths as documentation.

```
$ cat .gitattributes
project-docs/* linguist-documentation
docs/formatter.rb linguist-documentation=false
```

### Using Emacs or Vim modelines

Alternatively, you can use Vim or Emacs style modelines to set the language for a single file. Modelines can be placed anywhere within a file and are respected when determining how to syntax-highlight a file on GitHub.com

##### Vim
```
vim: set filetype=prolog:
vim: set ft=cpp:
```

##### Emacs
```
-*- mode: php;-*-
```

## Usage

Install the gem:

```
$ gem install github-linguist
```

Then use it in your application:

```ruby
require 'rugged'
require 'linguist'

repo = Rugged::Repository.new('.')
project = Linguist::Repository.new(repo, repo.head.target_id)
project.language       #=> "Ruby"
project.languages      #=> { "Ruby" => 119387 }
```

These stats are also printed out by the `linguist` executable. You can use the
`--breakdown` flag, and the binary will also output the breakdown of files by language.

You can try running `linguist` on the root directory in this repository itself:

```
$ bundle exec linguist --breakdown

100.00% Ruby

Ruby:
Gemfile
Rakefile
bin/linguist
github-linguist.gemspec
lib/linguist.rb
…
```

## Contributing

Please check out our [contributing guidelines](CONTRIBUTING.md).

##
