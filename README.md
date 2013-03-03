# Linguist

We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs and generate language breakdown graphs.

## Features

### Language detection

Linguist defines the list of all languages known to GitHub in a [yaml file](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml). In order for a file to be highlighted, a language and lexer must be defined there.

Most languages are detected by their file extension. This is the fastest and most common situation.

For disambiguating between files with common extensions, we use a [Bayesian classifier](https://github.com/github/linguist/blob/master/lib/linguist/classifier.rb). For an example, this helps us tell the difference between `.h` files which could be either C, C++, or Obj-C.

In the actual GitHub app we deal with `Grit::Blob` objects. For testing, there is a simple `FileBlob` API.

```ruby

Linguist::FileBlob.new("lib/linguist.rb").language.name #=> "Ruby"

Linguist::FileBlob.new("bin/linguist").language.name #=> "Ruby"
```

See [lib/linguist/language.rb](https://github.com/github/linguist/blob/master/lib/linguist/language.rb) and [lib/linguist/languages.yml](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml).

### Syntax Highlighting

The actual syntax highlighting is handled by our Pygments wrapper, [pygments.rb](https://github.com/tmm1/pygments.rb). It also provides a [Lexer abstraction](https://github.com/tmm1/pygments.rb/blob/master/lib/pygments/lexer.rb) that determines which highlighter should be used on a file.

We typically run on a pre-release version of Pygments, [pygments.rb](https://github.com/tmm1/pygments.rb), to get early access to new lexers. The [languages.yml](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml) file is a dump of the lexers we have available on our server.

### Stats

The Language Graph you see on every repository is built by aggregating the languages of all repo's blobs. The top language in the graph determines the project's primary language. Collectively, these stats make up the [Top Languages](https://github.com/languages) page.

The repository stats API can be used on a directory:

```ruby
project = Linguist::Repository.from_directory(".")
project.language.name  #=> "Ruby"
project.languages      #=> { "Ruby" => 0.98, "Shell" => 0.02 }
```

These stats are also printed out by the binary. Try running `linguist` on itself:

    $ bundle exec linguist lib/
    100%  Ruby

#### Ignore vendored files

Checking other code into your git repo is a common practice. But this often inflates your project's language stats and may even cause your project to be labeled as another language. We are able to identify some of these files and directories and exclude them.

```ruby
Linguist::FileBlob.new("vendor/plugins/foo.rb").vendored? # => true
```

See [Linguist::BlobHelper#vendored?](https://github.com/github/linguist/blob/master/lib/linguist/blob_helper.rb) and [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml).

#### Generated file detection

Not all plain text files are true source files. Generated files like minified js and compiled CoffeeScript can be detected and excluded from language stats. As an extra bonus, these files are suppressed in Diffs.

```ruby
Linguist::FileBlob.new("underscore.min.js").generated? # => true
```

See [Linguist::BlobHelper#generated?](https://github.com/github/linguist/blob/master/lib/linguist/blob_helper.rb).

## Installation

github.com is usually running the latest version of the `github-linguist` gem that is released on [RubyGems.org](http://rubygems.org/gems/github-linguist).

But for development you are going to want to checkout out the source. To get it, clone the repo and run [Bundler](http://gembundler.com/) to install its dependencies.

    git clone https://github.com/github/linguist.git
    cd linguist/
    bundle install

To run the tests:

    bundle exec rake test

## Contributing

The majority of patches won't need to touch any Ruby code at all. The [master language list](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml) is just a configuration file.

Almost all bug fixes or new language additions should come with some additional code samples. Just drop them under [`samples/`](https://github.com/github/linguist/tree/master/samples) in the correct subdirectory and our test suite will automatically test them. In most cases you shouldn't need to add any new assertions.

### Testing

Sometimes getting the tests running can be too much work, especially if you don't have much Ruby experience. It's okay, be lazy and let our build bot [Travis](http://travis-ci.org/#!/github/linguist) run the tests for you. Just open a pull request and the bot will start cranking away.

Here's our current build status, which is hopefully green: [![Build Status](https://secure.travis-ci.org/github/linguist.png?branch=master)](http://travis-ci.org/github/linguist)
