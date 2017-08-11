# Contributing

Hi there! We're thrilled that you'd like to contribute to this project. Your help is essential for keeping it great. This project adheres to the [Contributor Covenant Code of Conduct](http://contributor-covenant.org/). By participating, you are expected to uphold this code.

The majority of contributions won't need to touch any Ruby code at all.

## Adding an extension to a language

We try only to add new extensions once they have some usage on GitHub. In most cases we prefer that extensions be in use in hundreds of repositories before supporting them in Linguist.

To add support for a new extension:

1. Add your extension to the language entry in [`languages.yml`][languages], keeping the extensions in alphabetical order.
1. Add at least one sample for your extension to the [samples directory][samples] in the correct subdirectory.
1. Open a pull request, linking to a [GitHub search result](https://github.com/search?utf8=%E2%9C%93&q=extension%3Aboot+NOT+nothack&type=Code&ref=searchresults) showing in-the-wild usage.

In addition, if this extension is already listed in [`languages.yml`][languages] then sometimes a few more steps will need to be taken:

1. Make sure that example `.yourextension` files are present in the [samples directory][samples] for each language that uses `.yourextension`.
1. Test the performance of the Bayesian classifier with a relatively large number (1000s) of sample `.yourextension` files. (ping **@bkeepers** to help with this) to ensure we're not misclassifying files.
1. If the Bayesian classifier does a bad job with the sample `.yourextension` files then a [heuristic](https://github.com/github/linguist/blob/master/lib/linguist/heuristics.rb) may need to be written to help.


## Adding a language

We try only to add languages once they have some usage on GitHub. In most cases we prefer that each new file extension be in use in hundreds of repositories before supporting them in Linguist.

To add support for a new language:

1. Add an entry for your language to [`languages.yml`][languages]. Omit the `language_id` field for now.
1. Add a grammar for your language: `script/add-grammar https://github.com/JaneSmith/MyGrammar`. Please only add grammars that have [one of these licenses][licenses].
1. Add samples for your language to the [samples directory][samples] in the correct subdirectory.
1. Add a `language_id` for your language using `script/set-language-ids`. **You should only ever need to run `script/set-language-ids --update`. Anything other than this risks breaking GitHub search :cry:**
1. Open a pull request, linking to a [GitHub search result](https://github.com/search?utf8=%E2%9C%93&q=extension%3Aboot+NOT+nothack&type=Code&ref=searchresults) showing in-the-wild usage.

In addition, if your new language defines an extension that's already listed in [`languages.yml`][languages] (such as `.foo`) then sometimes a few more steps will need to be taken:

1. Make sure that example `.foo` files are present in the [samples directory][samples] for each language that uses `.foo`.
1. Test the performance of the Bayesian classifier with a relatively large number (1000s) of sample `.foo` files. (ping **@bkeepers** to help with this) to ensure we're not misclassifying files.
1. If the Bayesian classifier does a bad job with the sample `.foo` files then a [heuristic](https://github.com/github/linguist/blob/master/lib/linguist/heuristics.rb) may need to be written to help.

Remember, the goal here is to try and avoid false positives!


## Fixing a misclassified language

Most languages are detected by their file extension defined in [languages.yml][languages].  For disambiguating between files with common extensions, linguist applies some [heuristics](/lib/linguist/heuristics.rb) and a [statistical classifier](lib/linguist/classifier.rb). This process can help differentiate between, for example, `.h` files which could be either C, C++, or Obj-C.

Misclassifications can often be solved by either adding a new filename or extension for the language or adding more [samples][samples] to make the classifier smarter.


## Fixing syntax highlighting

Syntax highlighting in GitHub is performed using TextMate-compatible grammars. These are the same grammars that TextMate, Sublime Text and Atom use. Every language in [languages.yml][languages] is mapped to its corresponding TM `scope`. This scope will be used when picking up a grammar for highlighting.

Assuming your code is being detected as the right language, in most cases this is due to a bug in the language grammar rather than a bug in Linguist. [`grammars.yml`][grammars] lists all the grammars we use for syntax highlighting on github.com. Find the one corresponding to your code's programming language and submit a bug report upstream. If you can, try to reproduce the highlighting problem in the text editor that the grammar is designed for (TextMate, Sublime Text, or Atom) and include that information in your bug report.

You can also try to fix the bug yourself and submit a Pull Request. [TextMate's documentation](https://manual.macromates.com/en/language_grammars) offers a good introduction on how to work with TextMate-compatible grammars. You can test grammars using [Lightshow](https://github-lightshow.herokuapp.com).

Once the bug has been fixed upstream, we'll pick it up for GitHub in the next release of Linguist.

## Testing

For development you are going to want to checkout out the source. To get it, clone the repo and run [Bundler](http://gembundler.com/) to install its dependencies.

    git clone https://github.com/github/linguist.git
    cd linguist/
    script/bootstrap

To run Linguist from the cloned repository, you will need to generate the code samples first:

    bundle exec rake samples

Run this command each time a [sample][samples] has been modified.

To run Linguist from the cloned repository:

    bundle exec bin/linguist --breakdown

To run the tests:

    bundle exec rake test

Sometimes getting the tests running can be too much work, especially if you don't have much Ruby experience. It's okay: be lazy and let our build bot [Travis](https://travis-ci.org/#!/github/linguist) run the tests for you. Just open a pull request and the bot will start cranking away.

Here's our current build status: [![Build Status](https://api.travis-ci.org/github/linguist.svg?branch=master)](https://travis-ci.org/github/linguist)

## Maintainers

Linguist is maintained with :heart: by:

- **@Alhadis**
- **@BenEddy** (GitHub staff)
- **@Caged** (GitHub staff)
- **@grantr** (GitHub staff)
- **@larsbrinkhoff**
- **@lildude** (GitHub staff)
- **@pchaigno**
- **@rafer** (GitHub staff)
- **@shreyasjoshis** (GitHub staff)

As Linguist is a production dependency for GitHub we have a couple of workflow restrictions:

- Anyone with commit rights can merge Pull Requests provided that there is a :+1: from a GitHub member of staff
- Releases are performed by GitHub staff so we can ensure GitHub.com always stays up to date with the latest release of Linguist and there are no regressions in production.

### Releasing

If you are the current maintainer of this gem:

1. Create a branch for the release: `git checkout -b cut-release-vxx.xx.xx`
1. Make sure your local dependencies are up to date: `script/bootstrap`
1. If grammar submodules have not been updated recently, update them: `git submodule update --remote && git commit -a`
1. Ensure that samples are updated: `bundle exec rake samples`
1. Ensure that tests are green: `bundle exec rake test`
1. Bump gem version in `lib/linguist/version.rb`, [like this](https://github.com/github/linguist/commit/8d2ea90a5ba3b2fe6e1508b7155aa4632eea2985).
1. Make a PR to github/linguist, [like this](https://github.com/github/linguist/pull/1238).
1. Build a local gem: `bundle exec rake build_gem`
1. Test the gem:
  1. Bump the Gemfile and Gemfile.lock versions for an app which relies on this gem
  1. Install the new gem locally
  1. Test behavior locally, branch deploy, whatever needs to happen
1. Merge github/linguist PR
1. Tag and push: `git tag vx.xx.xx; git push --tags`
1. Push to rubygems.org -- `gem push github-linguist-3.0.0.gem`

[grammars]: /grammars.yml
[languages]: /lib/linguist/languages.yml
[licenses]: https://github.com/github/linguist/blob/257425141d4e2a5232786bf0b13c901ada075f93/vendor/licenses/config.yml#L2-L11
[samples]: /samples
[new-issue]: https://github.com/github/linguist/issues/new
