# Linguist

[![Build Status](https://travis-ci.org/github/linguist.svg?branch=master)](https://travis-ci.org/github/linguist)

[issues]: https://github.com/github/linguist/issues
[new-issue]: https://github.com/github/linguist/issues/new

This library is used on GitHub.com to detect blob languages, ignore binary or vendored files, suppress generated files in diffs, and generate language breakdown graphs.

See [Troubleshooting](#troubleshooting) and [`CONTRIBUTING.md`](CONTRIBUTING.md) before filing an issue or creating a pull request.

## How Linguist works

Linguist takes the list of languages it knows from [`languages.yml`](/lib/linguist/languages.yml) and uses a number of methods to try and determine the language used by each file, and the overall repository breakdown.

Linguist starts by going through all the files in a repository and excludes all files that it determines to be binary data, [vendored code](#vendored-code), [generated code](#generated-code), [documentation](#documentation), or are defined as `data` (e.g. SQL) or `prose` (e.g. Markdown) languages, whilst taking into account any [overrides](#overrides).

If an [explicit language override](#using-gitattributes) has been used, that language is used for the matching files. The language of each remaining file is then determined using the following strategies, in order, with each step either identifying the precise language or reducing the number of likely languages passed down to the next strategy:

- Vim or Emacs modeline,
- commonly used filename,
- shell shebang,
- file extension,
- XML header,
- heuristics,
- naïve Bayesian classification

The result of this analysis is used to produce the language stats bar which displays the languages percentages for the files in the repository. The percentages are calculated based on the bytes of code for each language as reported by the [List Languages](https://developer.github.com/v3/repos/#list-languages) API.

![language stats bar](https://cloud.githubusercontent.com/assets/173/5562290/48e24654-8ddf-11e4-8fe7-735b0ce3a0d3.png)

### How Linguist works on GitHub.com

When you push changes to a repository on GitHub.com, a low priority background job is enqueued to analyze your repository as explained above. The results of this analysis are cached for the lifetime of your repository and are only updated when the repository is updated. As this analysis is performed by a low priority background job, it can take a while, particularly during busy periods, for your language statistics bar to reflect your changes.


## Usage

### Installation

Install the gem:

    $ gem install github-linguist

#### Dependencies

Linguist uses [`charlock_holmes`](https://github.com/brianmario/charlock_holmes) for character encoding and [`rugged`](https://github.com/libgit2/rugged) for libgit2 bindings for Ruby. These components have their own dependencies.
1. charlock_holmes
    * cmake
    * pkg-config
    * [ICU](http://site.icu-project.org/)
    * [zlib](https://zlib.net/)
2. rugged
    * [libcurl](https://curl.haxx.se/libcurl/)
    * [OpenSSL](https://www.openssl.org)

You may need to install missing dependencies before you can install Linguist. For example, on macOS with [Homebrew](http://brew.sh/): `brew install cmake pkg-config icu4c` and on Ubuntu: `sudo apt-get install cmake pkg-config libicu-dev zlib1g-dev libcurl4-openssl-dev libssl-dev`.

### Application usage

Linguist can be used in your application as follows:

```ruby
require 'rugged'
require 'linguist'

repo = Rugged::Repository.new('.')
project = Linguist::Repository.new(repo, repo.head.target_id)
project.language       #=> "Ruby"
project.languages      #=> { "Ruby" => 119387 }
```

### Command line usage

A repository's languages stats can also be assessed from the command line using the `linguist` executable. Without any options, `linguist` will output the breakdown that correlates to what is shown in the language stats bar. The `--breakdown` flag will additionally show the breakdown of files by language.

```console
$ cd /path-to-repository/
$ github-linguist
```

You can try running `linguist` on the root directory in this repository itself:

```console
$ bundle exec bin/github-linguist --breakdown
68.57%  Ruby
22.90%  C
6.93%   Go
1.21%   Lex
0.39%   Shell

Ruby:
Gemfile
Rakefile
bin/git-linguist
bin/github-linguist
ext/linguist/extconf.rb
github-linguist.gemspec
lib/linguist.rb
…
```


## Troubleshooting

Please check [the FAQ](FAQ.md) for any issue you may have.


## Overrides

Linguist supports a number of different custom override strategies for language definitions and file paths.

### Using gitattributes

Add a `.gitattributes` file to your project and use standard git-style path matchers for the files you want to override using the `linguist-documentation`, `linguist-language`, `linguist-vendored`, `linguist-generated`  and `linguist-detectable` attributes. `.gitattributes` will be used to determine language statistics and will be used to syntax highlight files. You can also manually set syntax highlighting using [Vim or Emacs modelines](#using-emacs-or-vim-modelines).

When testing with a local installation of Linguist, take note that the added attributes will not take effect until the .gitattributes file is commited to your repository.

File and folder paths inside .gitattributes are calculated relative to the position of the .gitattributes file.

```
$ cat .gitattributes
*.rb linguist-language=Java
```

#### Vendored code

Checking code you didn't write, such as JavaScript libraries, into your git repo is a common practice, but this often inflates your project's language stats and may even cause your project to be labeled as another language. By default, Linguist treats all of the paths defined in [`vendor.yml`](/lib/linguist/vendor.yml) as vendored and therefore doesn't include them in the language statistics for a repository.

Use the `linguist-vendored` attribute to vendor or un-vendor paths.

```
$ cat .gitattributes
special-vendored-path/* linguist-vendored
jquery.js linguist-vendored=false
```

#### Documentation

Just like vendored files, Linguist excludes documentation files from your project's language stats. [`documentation.yml`](/lib/linguist/documentation.yml) lists common documentation paths and excludes them from the language statistics for your repository.

Use the `linguist-documentation` attribute to mark or unmark paths as documentation.

```
$ cat .gitattributes
project-docs/* linguist-documentation
docs/formatter.rb linguist-documentation=false
```

#### Generated code

Not all plain text files are true source files. Generated files like minified JavaScript and compiled CoffeeScript can be detected and excluded from language stats. As an added bonus, unlike vendored and documentation files, these files are suppressed in diffs. [`generated.rb`](/lib/linguist/generated.rb) lists common generated paths and excludes them from the language statistics of your repository.

Use the `linguist-generated` attribute to mark or unmark paths as generated.

```
$ cat .gitattributes
Api.elm linguist-generated=true
```

#### Detectable

Only programming languages are included in the language statistics. Languages of a different type (as defined in [`languages.yml`](/lib/linguist/languages.yml)) are not "detectable" causing them not to be included in the language statistics.

Use the `linguist-detectable` attribute to mark or unmark paths as detectable.

```
$ cat .gitattributes
*.kicad_pcb linguist-detectable=true
*.sch linguist-detectable=true
tools/export_bom.py linguist-detectable=false
```

### Using Emacs or Vim modelines

If you do not want to use `.gitattributes` to override the syntax highlighting used on GitHub.com, you can use Vim or Emacs style modelines to set the language for a single file. Modelines can be placed anywhere within a file and are respected when determining how to syntax-highlight a file on GitHub.com

##### Vim
```
# Some examples of various styles:
vim: syntax=java
vim: set syntax=ruby:
vim: set filetype=prolog:
vim: set ft=cpp:
```

##### Emacs
```
-*- mode: php;-*-
```


## Contributing

Please check out our [contributing guidelines](CONTRIBUTING.md).


## License

The language grammars included in this gem are covered by their repositories'
respective licenses. [`vendor/README.md`](/vendor/README.md) lists the repository for each grammar.

All other files are covered by the MIT license, see `LICENSE`.
