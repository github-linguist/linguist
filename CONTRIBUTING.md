# Contributing

The majority of contributions won't need to touch any Ruby code at all. Linguist defines a list of all languages known to GitHub in [lib/linguist/languages.yml](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml).

Most languages are detected by their file extension. For disambiguating between files with common extensions, we first apply some common-sense heuristics to pick out obvious languages. After that, we use a [statistical classifier](https://github.com/github/linguist/blob/master/lib/linguist/classifier.rb). This process can help us tell the difference between, for example, `.h` files which could be either C, C++, or Obj-C.

## Adding a language

We try only to add languages once they have some usage on GitHub. In most cases we prefer that languages be in use in hundreds of repositories before supporting them in Linguist.

To add support for a new language:

0. Add an entry for your language to [`languages.yml`][languages].
0. Add a grammar for your language. Please only add grammars that have a license that permits redistribution.
  0. Add your grammar as a submodule: `git submodule add https://github.com/JaneSmith/MyGrammar vendor/grammars/MyGrammar`.
  0. Add your grammar to [`grammars.yml`][grammars] by running `script/download-grammars --add vendor/grammars/MyGrammar`.
0. Add samples for your language to the [samples directory][samples] in the correct subdirectory.
0. Open a pull request, linking to a [GitHub search result](https://github.com/search?utf8=%E2%9C%93&q=extension%3Aboot+NOT+nothack&type=Code&ref=searchresults) showing in-the-wild usage.

## Fixing syntax highlighting

Syntax highlighting in GitHub is performed using TextMate-compatible grammars. These are the same grammars that TextMate, Sublime Text and Atom use. Every language in `languages.yml` is mapped to its corresponding TM `scope`. This scope will be used when picking up a grammar for highlighting.

Assuming your code is being detected as the right language (see [Language Detection](#language-detection) above), in most cases this is due to a bug in the language grammar rather than a bug in Linguist. [`grammars.yml`][grammars] lists all the grammars we use for syntax highlighting on github.com. Find the one corresponding to your code's programming language and submit a bug report upstream. If you can, try to reproduce the highlighting problem in the text editor that the grammar is designed for (TextMate, Sublime Text, or Atom) and include that information in your bug report.

You can also try to fix the bug yourself and submit a Pull Request. [TextMate's documentation](http://manual.macromates.com/en/language_grammars) offers a good introduction on how to work with TextMate-compatible grammars. You can test grammars using [Lightshow](https://lightshow.githubapp.com).

Once the bug has been fixed upstream, please let us know and we'll pick it up for GitHub.

## Testing

For development you are going to want to checkout out the source. To get it, clone the repo and run [Bundler](http://gembundler.com/) to install its dependencies.

    git clone https://github.com/github/linguist.git
    cd linguist/
    script/bootstrap

To run the tests:

    bundle exec rake test

Sometimes getting the tests running can be too much work, especially if you don't have much Ruby experience. It's okay: be lazy and let our build bot [Travis](http://travis-ci.org/#!/github/linguist) run the tests for you. Just open a pull request and the bot will start cranking away.

Here's our current build status: [![Build Status](https://secure.travis-ci.org/github/linguist.png?branch=master)](http://travis-ci.org/github/linguist)

## Releasing

If you are the current maintainer of this gem:

0. Create a branch for the release: `git checkout -b cut-release-vxx.xx.xx`
0. Make sure your local dependencies are up to date: `script/bootstrap`
0. Ensure that samples are updated: `bundle exec rake samples`
0. Ensure that tests are green: `bundle exec rake test`
0. Bump gem version in `lib/linguist/version.rb`, [like this](https://github.com/github/linguist/commit/8d2ea90a5ba3b2fe6e1508b7155aa4632eea2985).
0. Make a PR to github/linguist, [like this](https://github.com/github/linguist/pull/1238).
0. Build a local gem: `bundle exec rake build_gem`
0. Test the gem:
  0. Bump the Gemfile and Gemfile.lock versions for an app which relies on this gem
  0. Install the new gem locally
  0. Test behavior locally, branch deploy, whatever needs to happen
0. Merge github/linguist PR
0. Tag and push: `git tag vx.xx.xx; git push --tags`
0. Push to rubygems.org -- `gem push github-linguist-3.0.0.gem`

[grammars]: /grammars.yml
[languages]: /lib/linguist/languages.yml
[samples]: /samples
[new-issue]: https://github.com/github/linguist/issues/new
