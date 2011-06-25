# Linguist

We use this library at GitHub to detect blob languages, highlight code, ignore binary files, suppress generated files in diffs and generate language breakdown graphs.

## Features

### Language detection

Linguist defines the list of all languages known to GitHub in a [yaml file](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml). In order for a file to be hightlighed, a language and lexer must be defined there.

Most languages are detected by their file extension. This is the fastest and most common situation. For script files, which are usually extensionless, we do "deep content inspection"™ and check the shebang of the file. Checking the file's contents may also be used for disambiguating languages. C, C++ and Obj-C all use `.h` files. Looking for common keywords, we are usually able to guess the correct language.

In the actual GitHub app we deal with `Grit::Blob` objects. For testing, there is a simple `FileBlob` API.

    file = Linguist::FileBlob.new("lib/linguist.rb")
    file.language.name #=> "Ruby"

    file = Linguist::FileBlob.new("bin/linguist")
    file.language.name #=> "Ruby"

See [lib/linguist/language.rb](https://github.com/github/linguist/blob/master/lib/linguist/language.rb) and [lib/linguist/languages.yml](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml).

#### Syntax Highlighting

The actual syntax highlighting is handled by our Pygments wrapper, [Albino](https://github.com/github/albino). Linguist provides a [Lexer abstraction](https://github.com/github/linguist/blob/master/lib/linguist/lexer.rb) that determines which highlighter should be used on a file.

We typical run on a prerelease version of Pygments to get early access to new lexers. The [lexers.yml](https://github.com/github/linguist/blob/master/lib/linguist/lexers.yml) file is a dump of the lexers we have available on our server. If there is a new lexer in pygments-main not on the list, [open an issue](https://github.com/github/linguist/issues) and we'll try to upgrade it soon.

### MIME type detection

Most of the MIME types handling is done by the Ruby [mime-types gem](https://github.com/halostatue/mime-types/blob/master/lib/mime/types.rb.data). But we have our own list of additions and overrides. To add or modify this list, see [lib/linguist/mimes.yml](https://github.com/github/linguist/blob/master/lib/linguist/mimes.yml).

MIME types are used to set the Content-Type of raw binary blobs which are served from a special `raw.github.com` domain. However, all text blobs are served as `text/plain` regardless of their type to ensure they open in the browser rather than downloading.

The MIME type also determines whether a blob is binary or plain text. So if you're seeing a blob that says "View Raw" and it is actually plain text, the mime type and encoding probably needs to be explicitly stated.

    file = Linguist::FileBlob.new("linguist.zip")
    file.binary? #=> true

See [lib/linguist/mimes.yml](https://github.com/github/linguist/blob/master/lib/linguist/mimes.yml).

### Stats

The [Language Graph](https://github.com/github/linguist/graphs/languages) is built by aggregating the languages of all repo's blobs. The top language in the graph determines the project's primary language. Collectively, these stats make up the [Top Languages](https://github.com/languages) page.

The repository stats API can be used on a directory:

    project = Linguist::Repository.from_directory(".")
    project.language.name  #=> "Ruby"
    project.languages      #=> { "Ruby" => 0.98,
                                 "Shell" => 0.02 }

These stats are also printed out by the binary. Try running `linguist` on itself:

    $ bundle exec linguist lib/
    100%  Ruby

### Ignore vendored files

Checking other code into your git repo is a common practice. But this often inflates your project's language stats and may even cause your project to be labeled as another language. We are able to identify some of these files and directories and exclude them.

    file = Linguist::FileBlob.new("vendor/plugins/foo.rb")
    file.vendored? # => true

See [Linguist::BlobHelper#vendored?](https://github.com/github/linguist/blob/master/lib/linguist/blob_helper.rb) and [lib/linguist/vendor.yml](https://github.com/github/linguist/blob/master/lib/linguist/vendor.yml).

### Generated file detection

Not all plain text files are true source files. Generated files like minified js and compiled CoffeeScript can be detected and excluded from language stats. As an extra bonus, these files are suppressed in Diffs.

    file = Linguist::FileBlob.new("underscore.min.js")
    file.generated? # => true

See [Linguist::BlobHelper#generated?](https://github.com/github/linguist/blob/master/lib/linguist/blob_helper.rb).

## Contributing

1. Fork it.
2. Create a branch (`git checkout -b detect-foo-language`)
3. Make your changes
4. Run the tests (`bundle install` then `bundle exec rake`)
5. Commit your changes (`git commit -am "Added detection for the new Foo language"`)
6. Push to the branch (`git push origin detect-foo-language`)
7. Create a [Pull Request](http://help.github.com/pull-requests/) from your branch.
8. Promote it. Get others to drop in and +1 it.
