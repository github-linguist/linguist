# Linguist

We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs, and generate language breakdown graphs.

## Features

### Language detection

Linguist defines a list of all languages known to GitHub in a [yaml file](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml). In order for a file to be highlighted, a language and a lexer must be defined there.

Most languages are detected by their file extension. For disambiguating between files with common extensions, we first apply some common-sense heuristics to pick out obvious languages. After that, we use a
[statistical
classifier](https://github.com/github/linguist/blob/master/lib/linguist/classifier.rb).
This process can help us tell the difference between, for example, `.h` files which could be either C, C++, or Obj-C.

```ruby

Linguist::FileBlob.new("lib/linguist.rb").language.name #=> "Ruby"

Linguist::FileBlob.new("bin/linguist").language.name #=> "Ruby"
```

See [lib/linguist/language.rb](https://github.com/github/linguist/blob/master/lib/linguist/language.rb) and [lib/linguist/languages.yml](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml).

### Syntax Highlighting

The actual syntax highlighting is handled by our Pygments wrapper, [pygments.rb](https://github.com/tmm1/pygments.rb). It also provides a [Lexer abstraction](https://github.com/tmm1/pygments.rb/blob/master/lib/pygments/lexer.rb) that determines which highlighter should be used on a file.

### Stats

The Language stats bar that you see on every repository is built by aggregating the languages of each file in that repository. The top language in the graph determines the project's primary language.

The repository stats API, accessed through `#languages`, can be used on a directory:

```ruby
project = Linguist::Repository.from_directory(".")
project.language.name  #=> "Ruby"
project.languages      #=> { "Ruby" => 0.98, "Shell" => 0.02 }
```

These stats are also printed out by the `linguist` binary. You can use the
`--breakdown` flag, and the binary will also output the breakdown of files by language.

You can try running `linguist` on the `lib/` directory in this repository itself:

    $ bundle exec linguist lib/ --breakdown

    100.00% Ruby

    Ruby:
    linguist/blob_helper.rb
    linguist/classifier.rb
    linguist/file_blob.rb
    linguist/generated.rb
    linguist/heuristics.rb
    linguist/language.rb
    linguist/md5.rb
    linguist/repository.rb
    linguist/samples.rb
    linguist/tokenizer.rb
    linguist.rb

#### Ignore vendored files

Checking other code into your git repo is a common practice. But this often inflates your project's language stats and may even cause your project to be labeled as another language. We are able to identify some of these files and directories and exclude them.

```ruby
Linguist::FileBlob.new("vendor/plugins/foo.rb").vendored? # => true
```

See [Linguist::BlobHelper#vendored?](https://github.com/github/linguist/blob/master/lib/linguist/blob_helper.rb) and [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml).

#### Generated file detection

Not all plain text files are true source files. Generated files like minified js and compiled CoffeeScript can be detected and excluded from language stats. As an extra bonus, these files are suppressed in diffs.

```ruby
Linguist::FileBlob.new("underscore.min.js").generated? # => true
```

See [Linguist::Generated#generated?](https://github.com/github/linguist/blob/master/lib/linguist/generated.rb).

## Installation

github.com is usually running the latest version of the `github-linguist` gem that is released on [RubyGems.org](http://rubygems.org/gems/github-linguist).

But for development you are going to want to checkout out the source. To get it, clone the repo and run [Bundler](http://gembundler.com/) to install its dependencies.

    git clone https://github.com/github/linguist.git
    cd linguist/
    bundle install

To run the tests:

    bundle exec rake test

## Contributing

The majority of contributions won't need to touch any Ruby code at all. The [master language list](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml) is just a YAML configuration file.

We try to only add languages once they have some usage on GitHub, so please note in-the-wild usage examples in your pull request.

Almost all bug fixes or new language additions should come with some additional code samples. Just drop them under [`samples/`](https://github.com/github/linguist/tree/master/samples) in the correct subdirectory and our test suite will automatically test them. In most cases you shouldn't need to add any new assertions.

### A note on language extensions

Linguist has a number of methods available to it for identifying the language of a particular file. The initial lookup is based upon the extension of the file, possible file extensions are defined in an array called `extensions`. Take a look at this example for example for `Perl`:

```
Perl:
  type: programming
  ace_mode: perl
  color: "#0298c3"
  extensions:
  - .pl
  - .PL
  - .perl
  - .ph
  - .plx
  - .pm
  - .pod
  - .psgi
  interpreters:
  - perl
```
Any of the extensions defined are valid but the first in this array should be the most popular.

### Testing

Sometimes getting the tests running can be too much work, especially if you don't have much Ruby experience. It's okay: be lazy and let our build bot [Travis](http://travis-ci.org/#!/github/linguist) run the tests for you. Just open a pull request and the bot will start cranking away.

Here's our current build status, which is hopefully green: [![Build Status](https://secure.travis-ci.org/github/linguist.png?branch=master)](http://travis-ci.org/github/linguist)

### Releasing

If you are the current maintainer of this gem:

 0. Create a branch for the release: `git checkout -b cut-release-vxx.xx.xx`
 0. Make sure your local dependencies are up to date: `bundle install`
 0. Ensure that samples are updated: `bundle exec rake samples`
 0. Ensure that tests are green: `bundle exec rake test`
 0. Bump gem version in `lib/linguist/version.rb`.  For example, [like this](https://github.com/github/linguist/commit/8d2ea90a5ba3b2fe6e1508b7155aa4632eea2985).
 0. Make a PR to github/linguist.  For example, [#1238](https://github.com/github/linguist/pull/1238).
 0. Build a local gem: `gem build github-linguist.gemspec`
 0. Testing:
   0. Bump the Gemfile and Gemfile.lock versions for an app which relies on this gem
   0. Install the new gem locally
   0. Test behavior locally, branch deploy, whatever needs to happen
 0. Merge github/linguist PR
 0. Tag and push: `git tag vx.xx.xx; git push --tags`
 0. Push to rubygems.org -- `gem push github-linguist-3.0.0.gem`
