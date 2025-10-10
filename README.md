# Linguist

[![Actions Status](https://github.com/github/linguist/workflows/Run%20Tests/badge.svg)](https://github.com/github/linguist/actions) 

[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/github-linguist/linguist)

This library is used on GitHub.com to detect blob languages, ignore binary or vendored files, suppress generated files in diffs, and generate language breakdown graphs.

## Documentation

- [How Linguist works](/docs/how-linguist-works.md)
- [Change Linguist's behaviour with overrides](/docs/overrides.md)
- [Troubleshooting](/docs/troubleshooting.md)
- [Contributing guidelines](CONTRIBUTING.md)

## Installation

Install the gem:

```bash
gem install github-linguist
```

### Dependencies

Linguist is a Ruby library so you will need a recent version of Ruby installed.
There are known problems with the macOS/Xcode supplied version of Ruby that causes problems installing some of the dependencies.
Accordingly, we highly recommend you install a version of Ruby using Homebrew, `rbenv`, `rvm`, `ruby-build`, `asdf` or other packaging system, before attempting to install Linguist and the dependencies.

Linguist uses [`charlock_holmes`](https://github.com/brianmario/charlock_holmes) for character encoding and [`rugged`](https://github.com/libgit2/rugged) for libgit2 bindings for Ruby.
These components have their own dependencies.

1. charlock_holmes
    * cmake
    * pkg-config
    * [ICU](http://site.icu-project.org/)
    * [zlib](https://zlib.net/)
2. rugged
    * [libcurl](https://curl.haxx.se/libcurl/)
    * [OpenSSL](https://www.openssl.org)

You may need to install missing dependencies before you can install Linguist.
For example, on macOS with [Homebrew](http://brew.sh/):

```bash
brew install cmake pkg-config icu4c
```

On Ubuntu:

```bash
sudo apt-get install build-essential cmake pkg-config libicu-dev zlib1g-dev libcurl4-openssl-dev libssl-dev ruby-dev
```

## Usage

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

The `github-linguist` executable operates in two distinct modes:

1. **[Git Repository mode](#git-repository)** - Analyzes an entire Git repository (when given a directory path or no path)
2. **[Single file mode](#single-file)** - Analyzes a specific file (when given a file path)

#### Git Repository

A repository's languages stats can be assessed from the command line using the `github-linguist` executable.
Without any options, `github-linguist` will output the language breakdown by percentage and file size.

```bash
cd /path-to-repository
github-linguist
```

You can try running `github-linguist` on the root directory in this repository itself:

```console
$ github-linguist
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
```

#### Additional options

##### `--rev REV`

The `--rev REV` flag will change the git revision being analyzed to any [gitrevisions(1)](https://git-scm.com/docs/gitrevisions#_specifying_revisions) compatible revision you specify.

This is useful to analyze the makeup of a repo as of a certain tag, or in a certain branch.

For example, here is the popular [Jekyll open source project](https://github.com/jekyll/jekyll).

```console
$ github-linguist jekyll

70.64%  709959     Ruby
23.04%  231555     Gherkin
3.80%   38178      JavaScript
1.19%   11943      HTML
0.79%   7900       Shell
0.23%   2279       Dockerfile
0.13%   1344       Earthly
0.10%   1019       CSS
0.06%   606        SCSS
0.02%   234        CoffeeScript
0.01%   90         Hack
```

And here is Jekyll's published website, from the gh-pages branch inside their repository.

```console
$ github-linguist jekyll --rev origin/gh-pages
100.00% 2568354    HTML
```

##### `--breakdown`

The `--breakdown` or `-b` flag will additionally show the breakdown of files by language.

You can try running `github-linguist` on the root directory in this repository itself:

```console
$ github-linguist --breakdown
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile

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

##### `--strategies`

The `--strategies` or `-s` flag will show the language detection strategy used for each file. This is useful for understanding how Linguist determined the language of specific files. Note that unless the `--json` flag is specified, this flag will set the `--breakdown` flag implicitly.

You can try running `github-linguist` on the root directory in this repository itself with the strategies flag:

```console
$ github-linguist --breakdown --strategies
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile

Ruby:
  Gemfile [Filename]
  Rakefile [Filename]
  bin/git-linguist [Extension]
  bin/github-linguist [Extension]
  lib/linguist.rb [Extension]
  …
```

If a file's language is affected by `.gitattributes`, the strategy will show the original detection method along with a note indicating whether the gitattributes setting changed the result or confirmed it.

For instance, if you had the following .gitattributes overrides in your repo:

```gitattributes

*.ts linguist-language=JavaScript
*.js linguist-language=JavaScript

```

the output of Linguist would be something like this:

```console
100.00% 217        JavaScript

JavaScript:
  demo.ts [Heuristics (overridden by .gitattributes)]
  demo.js [Extension (confirmed by .gitattributes)]
```

##### `--json`

The `--json` or `-j` flag output the data into JSON format.

```console
$ github-linguist --json
{"Dockerfile":{"size":1212,"percentage":"0.31"},"Ruby":{"size":264519,"percentage":"66.84"},"C":{"size":97685,"percentage":"24.68"},"Lex":{"size":5098,"percentage":"1.29"},"Shell":{"size":1257,"percentage":"0.32"},"Go":{"size":25999,"percentage":"6.57"}}
```

This option can be used in conjunction with `--breakdown` to get a full list of files along with the size and percentage data.

```console
$ github-linguist --breakdown --json
{"Dockerfile":{"size":1212,"percentage":"0.31","files":["Dockerfile","tools/grammars/Dockerfile"]},"Ruby":{"size":264519,"percentage":"66.84","files":["Gemfile","Rakefile","bin/git-linguist","bin/github-linguist","ext/linguist/extconf.rb","github-linguist.gemspec","lib/linguist.rb",...]}}

```

NB. The `--strategies` flag has no effect, when the `--json` flag is present.

#### Single file

Alternatively you can find stats for a single file using the `github-linguist` executable.

You can try running `github-linguist` on files in this repository itself:

```console
$ github-linguist grammars.yml
grammars.yml: 884 lines (884 sloc)
  type:      Text
  mime type: text/x-yaml
  language:  YAML
```

#### Additional options

##### `--breakdown`

This flag has no effect in *Single file* mode.

##### `--strategies`

When using the `--strategies` or `-s` flag with a single file, you can see which detection method was used:

```console
$ github-linguist --strategies lib/linguist.rb 
lib/linguist.rb: 105 lines (96 sloc)
  type:      Text
  mime type: application/x-ruby
  language:  Ruby
  strategy:  Extension
```

If a file's language is affected by `.gitattributes`, the strategy will show whether the gitattributes setting changed the result or confirmed it:

In this fictitious example, it says "confirmed by .gitattributes" since the detection process (using the Filename strategy) would have given the same output as the override:
```console
.devcontainer/devcontainer.json: 27 lines (27 sloc)
  type:      Text
  mime type: application/json
  language:  JSON with Comments
  strategy:  Filename (confirmed by .gitattributes)
```

In this other fictitious example, it says "overridden by .gitattributes" since the gitattributes setting changes the detected language to something different:

```console
test.rb: 13 lines (11 sloc)
  type:      Text
  mime type: application/x-ruby
  language:  Java
  strategy:  Extension (overridden by .gitattributes)
```

Here, the `.rb` file would normally be detected as Ruby by the Extension strategy, but `.gitattributes` overrides it to be detected as Java instead.

##### `--json`

Using the `--json` flag will give you the output for a single file in JSON format:

```console
$ github-linguist --strategies --json  lib/linguist.rb
{"lib/linguist.rb":{"lines":105,"sloc":96,"type":"Text","mime_type":"application/x-ruby","language":"Ruby","large":false,"generated":false,"vendored":false}}
```

NB. The `--strategies` has no effect, when the `--json` flag is present.

#### Docker

If you have Docker installed you can either build or use
our pre-built images and run Linguist within a container:

```console
$ docker run --rm -v $(pwd):$(pwd):Z -w $(pwd) -t ghcr.io/github-linguist/linguist:latest
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
```

##### Building the image

```console
$ docker build -t linguist .
$ docker run --rm -v $(pwd):$(pwd):Z -w $(pwd) -t linguist
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
$ docker run --rm -v $(pwd):$(pwd) -w $(pwd) -t linguist github-linguist --breakdown
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile

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

## Contributing

Please check out our [contributing guidelines](CONTRIBUTING.md).

## License

The language grammars included in this gem are covered by their repositories' respective licenses.
[`vendor/README.md`](/vendor/README.md) lists the repository for each grammar.

All other files are covered by the MIT license, see [`LICENSE`](./LICENSE).
