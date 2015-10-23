# Contributing

Hi there! We're thrilled that you'd like to contribute to this project. Your help is essential for keeping it great. The majority of contributions won't need to touch any Ruby code at all.

## Adding a language

We try only to add languages once they have some usage on GitHub. In most cases we prefer that languages be in use in hundreds of repositories before supporting them in Linguist.

To add support for a new language:

0. Add an entry for your language to [`languages.yml`][languages].
0. Add a grammar for your language. Please only add grammars that have a license that permits redistribution.
  0. Add your grammar as a submodule: `git submodule add https://github.com/JaneSmith/MyGrammar vendor/grammars/MyGrammar`.
  0. Add your grammar to [`grammars.yml`][grammars] by running `script/convert-grammars --add vendor/grammars/MyGrammar`.
0. Add samples for your language to the [samples directory][samples] in the correct subdirectory.
0. Open a pull request, linking to a [GitHub search result](https://github.com/search?utf8=%E2%9C%93&q=extension%3Aboot+NOT+nothack&type=Code&ref=searchresults) showing in-the-wild usage.

In addition, if your new language defines an extension that's already listed in [`languages.yml`][languages] (such as `.foo`) then sometimes a few more steps will need to be taken:

0. Make sure that example `.foo` files are present in the [samples directory][samples] for each language that uses `.foo`.
0. Test the performance of the Bayesian classifier with a relatively large number (1000s) of sample `.foo` files. (ping @arfon or @bkeepers to help with this) to ensure we're not misclassifying files.
0. If the Bayesian classifier does a bad job with the sample `.foo` files then a [heuristic](https://github.com/github/linguist/blob/master/lib/linguist/heuristics.rb) may need to be written to help.

Remember, the goal here is to try and avoid false positives!

## Fixing a misclassified language

Most languages are detected by their file extension defined in [languages.yml][languages].  For disambiguating between files with common extensions, linguist applies some [heuristics](/lib/linguist/heuristics.rb) and a [statistical classifier](lib/linguist/classifier.rb). This process can help differentiate between, for example, `.h` files which could be either C, C++, or Obj-C.

Misclassifications can often be solved by either adding a new filename or extension for the language or adding more [samples][samples] to make the classifier smarter.

## Fixing syntax highlighting

Syntax highlighting in GitHub is performed using TextMate-compatible grammars. These are the same grammars that TextMate, Sublime Text and Atom use. Every language in [languages.yml][languages] is mapped to its corresponding TM `scope`. This scope will be used when picking up a grammar for highlighting.

Assuming your code is being detected as the right language, in most cases this is due to a bug in the language grammar rather than a bug in Linguist. [`grammars.yml`][grammars] lists all the grammars we use for syntax highlighting on github.com. Find the one corresponding to your code's programming language and submit a bug report upstream. If you can, try to reproduce the highlighting problem in the text editor that the grammar is designed for (TextMate, Sublime Text, or Atom) and include that information in your bug report.

You can also try to fix the bug yourself and submit a Pull Request. [TextMate's documentation](http://manual.macromates.com/en/language_grammars) offers a good introduction on how to work with TextMate-compatible grammars. You can test grammars using [Lightshow](https://github-lightshow.herokuapp.com).

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
0. If grammar submodules have not been updated recently, update them: `git submodule update --remote && git commit -a`
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
