# Linguist

We use this library at GitHub to detect blob languages, ignore binary files, suppress generated files in diffs, and generate language breakdown graphs.

Tips for filing issues and creating pull requests can be found in [`CONTRIBUTING.md`](/CONTRIBUTING.md).

## Features

### Language detection

Linguist defines a list of all languages known to GitHub in a [yaml file](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml).

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

Syntax highlighting in GitHub is performed using TextMate-compatible grammars. These are the same grammars that TextMate, Sublime Text and Atom use.

Every language in `languages.yml` is mapped to its corresponding TM `scope`. This scope will be used when picking up a grammar for highlighting. **When adding a new language to Linguist, please add its corresponding scope too (assuming there's an existing TextMate bundle, Sublime Text package, or Atom package) so syntax highlighting works for it**.

### Stats

The Language stats bar that you see on every repository is built by aggregating the languages of each file in that repository. The top language in the graph determines the project's primary language.

The repository stats API, accessed through `#languages`, can be used on a directory:

***API UPDATE***

Since [Version 3.0.0](https://github.com/github/linguist/releases/tag/v3.0.0) Linguist expects a git repository (in the form of a [Rugged::Repository](https://github.com/libgit2/rugged#repositories)) to be passed when initializing `Linguist::Repository`.


```ruby
require 'rugged'
require 'linguist'

repo = Rugged::Repository.new('.')
project = Linguist::Repository.new(repo, repo.head.target_id)
project.language       #=> "Ruby"
project.languages      #=> { "Ruby" => 119387 }
```

These stats are also printed out by the `linguist` binary. You can use the
`--breakdown` flag, and the binary will also output the breakdown of files by language.

You can try running `linguist` on the root directory in this repository itself:

    $ bundle exec linguist --breakdown

    100.00% Ruby

    Ruby:
    Gemfile
    Rakefile
    bin/linguist
    github-linguist.gemspec
    lib/linguist.rb
    lib/linguist/blob_helper.rb
    lib/linguist/classifier.rb
    lib/linguist/file_blob.rb
    lib/linguist/generated.rb
    lib/linguist/heuristics.rb
    lib/linguist/language.rb
    lib/linguist/lazy_blob.rb
    lib/linguist/md5.rb
    lib/linguist/repository.rb
    lib/linguist/samples.rb
    lib/linguist/tokenizer.rb
    lib/linguist/version.rb
    test/test_blob.rb
    test/test_classifier.rb
    test/test_heuristics.rb
    test/test_language.rb
    test/test_md5.rb
    test/test_pedantic.rb
    test/test_repository.rb
    test/test_samples.rb
    test/test_tokenizer.rb

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

## Overrides

Linguist supports a number of different custom overrides strategies for language definitions and vendored paths. 

### Using gitattributes

Add a `.gitattributes` file to your project using the keys `linguist-language` and `linguist-vendored` with the standard git-style path matchers for the files you want to override.

Please note that the overrides currently only affect the language statistics for a repository and not the syntax-highlighting of files.

```
$ cat .gitattributes
*.rb linguist-language=Java

$ linguist --breakdown
100.00% Java

Java:
ruby_file.rb
```

By default, Linguist treats all of the paths defined in [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml) as vendored and therefore doesn't include them in the language statistics for a repository. Use the `linguist-vendored` attribute to vendor or un-vendor paths.

```
$ cat .gitattributes
special-vendored-path/* linguist-vendored
jquery.js linguist-vendored=false
```

### Using Emacs and Vim modelines

Alternatively, you can use Vim and Emacs style modelines to set the language for a single file. Modelines can be placed anywhere within a file and are respected when determining how to syntax-highlight a file on GitHub.com

```
Vim
vim: set filetype=prolog: 
vim: set ft=cpp:

Emacs
-*- mode: php;-*-
```


## Installation

Github.com is usually running the latest version of the `github-linguist` gem that is released on [RubyGems.org](http://rubygems.org/gems/github-linguist).

But for development you are going to want to checkout out the source. To get it, clone the repo and run [Bundler](http://gembundler.com/) to install its dependencies.

    git clone https://github.com/github/linguist.git
    cd linguist/
    script/bootstrap

To run the tests:

    bundle exec rake test

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
 0. Make sure your local dependencies are up to date: `script/bootstrap`
 0. If grammar submodules have not been updated recently, update them: `git submodule update --remote && git commit -a`
 0. Ensure that samples are updated: `bundle exec rake samples`
 0. Ensure that tests are green: `bundle exec rake test`
 0. Bump gem version in `lib/linguist/version.rb`.  For example, [like this](https://github.com/github/linguist/commit/8d2ea90a5ba3b2fe6e1508b7155aa4632eea2985).
 0. Make a PR to github/linguist.  For example, [#1238](https://github.com/github/linguist/pull/1238).
 0. Build a local gem: `bundle exec rake build_gem`
 0. Testing:
   0. Bump the Gemfile and Gemfile.lock versions for an app which relies on this gem
   0. Install the new gem locally
   0. Test behavior locally, branch deploy, whatever needs to happen
 0. Merge github/linguist PR
 0. Tag and push: `git tag vx.xx.xx; git push --tags`
 0. Push to rubygems.org -- `gem push github-linguist-3.0.0.gem`
