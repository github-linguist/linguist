# Contributing

Hi there!
We're thrilled that you'd like to contribute to this project.
Your help is essential for keeping it great.

Contributions to this project are [released](https://docs.github.com/github/site-policy/github-terms-of-service#6-contributions-under-repository-license) to the public under the [project's open source license](LICENSE).

This project adheres to the [Contributor Covenant Code of Conduct](http://contributor-covenant.org/).
By participating, you are expected to uphold this code.

The majority of contributions won't need to touch any Ruby code at all.

## Setting up a working environment

In order to start contributing to Linguist, you will need to setup your working environment.
We detail three methods below and recommend using GitHub Codespaces as it is the easiest and quickest method that will provide you with a consistent pre-configured environment ready for you to start contributing.

### Using GitHub Codespaces

Using GitHub Codespaces is the easiest and quickest method to start contributing.
The Free and Pro plans for personal accounts include free use of GitHub Codespaces up to a fixed amount of usage every month.
For more info on GitHub Codespaces billing, see [this help page](https://docs.github.com/en/billing/managing-billing-for-github-codespaces/about-billing-for-github-codespaces).

To get started, click this button: [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/github-linguist/linguist)

### Using the dev container locally

To open locally in VS Code using a dev container, follow the [Visual Studio Code Dev Containers installation instructions](https://code.visualstudio.com/docs/remote/containers#_installation) to configure dev container support.
Close the repo and open it in VS Code.
VS Code will automatically offer to run this project in a dev container when the project is opened.
No additional setup will be required.

### Using your local system without the dev container

This is the longest and most involved and error-prone approach and is documented for those who can't use one of the methods above or prefer to do everything within the native operating system environment.

#### Dependencies

Linguist is a Ruby library so you will need a recent version of Ruby installed.
There are known problems with the macOS/XCode supplied version of Ruby that causes problems installing some of the dependencies.
Accordingly, we highly recommend you install a version of Ruby using Homebrew, `rbenv`, `rvm`, `ruby-build`, `asdf` or other packaging system, before attempting to install Linguist and the dependencies.

Linguist uses the [`charlock_holmes`](https://github.com/brianmario/charlock_holmes) character encoding detection library which in turn uses [ICU](http://site.icu-project.org/), and the libgit2 bindings for Ruby provided by [`rugged`](https://github.com/libgit2/rugged).
[Bundler](https://bundler.io/) v1.10.0 or newer is required for installing the Ruby gem dependencies.
[Docker](https://www.docker.com/) is also required when adding or updating grammars.
These components have their own dependencies - `icu4c`, and `cmake` and `pkg-config` respectively - which you may need to install before you can install Linguist.

On macOS with [Homebrew](http://brew.sh/) the instructions below under Getting started will install these dependencies for you.

On Ubuntu:

```bash
apt-get install cmake pkg-config libicu-dev docker.io ruby ruby-dev zlib1g-dev build-essential libssl-dev
```

The latest version of Bundler can be installed with `gem install bundler`.

#### Getting started

Before you can start contributing to Linguist, you'll need to set up your environment first.
Clone the repo and run `script/bootstrap` to install its dependencies.

```bash
git clone https://github.com/github/linguist.git
cd linguist/
script/bootstrap
```

To run Linguist from the cloned repository:

```bash
bundle exec bin/github-linguist --breakdown
```

## Adding an extension to a language

We try only to add new extensions once they have some usage on GitHub.
In most cases we prefer that each new file extension be in use in at least 200 unique `:user/:repo` repositories before supporting them in Linguist
(but see [#5756][] for a temporary change in the criteria).

To add support for a new extension:

1. Add your extension to the language entry in [`languages.yml`][languages].
   Keep the extensions in alphabetical order, sorted case-sensitively (uppercase before lowercase).
   The exception is the primary extension: it should always be first.
2. Add at least one sample for your extension to the [samples directory][samples] in the correct subdirectory.
   We prefer examples of real-world code showing common usage.
   The more representative of the structure of the language, the better.

   **"Hello world" examples will not be accepted.**
3. Open a pull request, linking to a [GitHub search result][search-example] showing in-the-wild usage.
   If you are adding a sample, please state clearly the license covering the code.
   If possible, link to the original source of the sample.
   If you wrote the sample specifically for the PR and are happy for it to be included under the MIT license that covers Linguist, you can state this instead.

Additionally, if this extension is already listed in [`languages.yml`][languages] and associated with another language, then a few more steps will need to be taken:

1. Make sure that at least two example `.yourextension` files are present in the [samples directory][samples] for each language that uses `.yourextension`.
2. If the two languages look vaguely similar, or one of the languages has uniquely identifiable characteristics, consider writing a [heuristic][] to help with the classification.

Remember, the goal here is to try and avoid false positives!

See [My Linguist PR has been merged but GitHub doesn't reflect my changes][merged-pr] for details on when your changes will appear on GitHub after your PR has been merged.

## Adding a language

We try only to add languages once they have some usage on GitHub.
In most cases we prefer that each new file extension be in use in at least 200 unique `:user/:repo` repositories before supporting them in Linguist
(but see [#5756][] for a temporary change in the criteria).

To add support for a new language:

1. Add an entry for your language to [`languages.yml`][languages].
   Omit the `language_id` field for now.
2. Add a syntax-highlighting grammar for your language using:

   ```bash
   script/add-grammar https://github.com/JaneSmith/MyGrammar
   ```

   This command will analyze the grammar and, if no problems are found, add it to the repository.
   If problems are found, please report them to the grammar maintainer as you will otherwise be unable to add it.

   **Please only add grammars that have [one of these licenses][licenses].**
3. Add samples for your language to the [samples directory][samples] in the correct subdirectory.
   We prefer examples of real-world code showing common usage.
   The more representative of the structure of the language, the better.

   **"Hello world" examples will not be accepted.**
4. Generate a unique ID for your language by running `script/update-ids`.
5. Open a pull request, linking to [GitHub search results][search-example] showing in-the-wild usage.
   Please state clearly the license covering the code in the samples.
   Link directly to the original source if possible.
   If you wrote the sample specifically for the PR and are happy for it to be included under the MIT license that covers Linguist, you can state this instead.

In addition, if your new language defines an extension that is already listed in [`languages.yml`][languages] and associated with another language, then a few more steps will need to be taken:

1. Make sure that at least two example `.foo` files are present in the [samples directory][samples] for each language that uses `.foo`.
2. If the two languages look vaguely similar, or one of the languages has uniquely identifiable characteristics, consider writing a [heuristic][] to help with the classification.

Remember, the goal here is to try and avoid false positives!

See [My Linguist PR has been merged but GitHub doesn't reflect my changes][merged-pr] for details on when your changes will appear on GitHub after your PR has been merged.

## Fixing a misclassified language

Most languages are detected by their file extension defined in [`languages.yml`][languages].
For disambiguating between files with common extensions, Linguist applies some [heuristics](/lib/linguist/heuristics.rb) and a [statistical classifier](lib/linguist/classifier.rb).
This process can help differentiate between, for example, `.h` files which could be either C, C++, or Obj-C.

Misclassifications can often be solved by either adding a new filename or extension for the language or adding more [samples][] to make the classifier smarter.

## Fixing syntax highlighting

Syntax highlighting in GitHub is performed using TextMate-compatible grammars.
These are the same grammars used by TextMate, Sublime Text, and Atom.
Every language in [`languages.yml`][languages] is mapped to its corresponding TextMate `scopeName`.
This scope name will be used when picking up a grammar for highlighting.

Assuming your code is being detected as the right language, in most cases syntax highlighting problems are due to a bug in the language grammar rather than a bug in Linguist.
[`vendor/README.md`][grammars] lists all the grammars we use for syntax highlighting on GitHub.com.
Find the one corresponding to your code's programming language and submit a bug report upstream.
If you can, try to reproduce the highlighting problem in the text editor that the grammar is designed for (TextMate, Sublime Text, or Atom) and include that information in your bug report.

You can also try to fix the bug yourself and submit a pull-request.
[TextMate's documentation](https://manual.macromates.com/en/language_grammars) offers a good introduction on how to work with TextMate-compatible grammars.
Note that Linguist uses [PCRE](https://www.pcre.org/) regular expressions, while TextMate uses [Oniguruma](https://github.com/kkos/oniguruma).
Although they are mostly compatible, there may be occasional differences in syntax and semantics between the two.
Linguist's grammar compiler will highlight any problems when the grammar is updated.

Once the bug has been fixed upstream, we'll pick it up for GitHub in the next release of Linguist.

See [My Linguist PR has been merged but GitHub doesn't reflect my changes][merged-pr] for details on when the upstream changes will appear on GitHub.

## Changing the source of a syntax highlighting grammar

We'd like to ensure Linguist and GitHub.com are using the latest and greatest grammars that are consistent with the current usage but understand that sometimes a grammar can lag behind the evolution of a language or even stop being developed.
This often results in someone grasping the opportunity to create a newer and better and more actively maintained grammar, and we'd love to use it and pass on its functionality to our users.

Switching the source of a grammar is really easy:

```bash
script/add-grammar --replace MyGrammar https://github.com/PeterPan/MyGrammar
```

This command will analyze the grammar and, if no problems are found, add it to the repository.
If problems are found, please report these problems to the grammar maintainer as you will not be able to add the grammar if problems are found.

**Please only add grammars that have [one of these licenses][licenses].**

Please then open a pull request for the updated grammar.

See [My Linguist PR has been merged but GitHub doesn't reflect my changes][merged-pr] for details on when your changes will appear on GitHub after your PR has been merged.

## Changing the color associated with a language

Many of the colors associated with the languages within Linguist have been in place for a very long time.
The colors were often chosen based on the colors used by the language at the time and since then users will have become familiar with those colors as they appear on GitHub.com.
If you would like to change the color of a language, we ask that you propose your suggested color change to the wider community for your language to gain consensus before submitting a pull request.
Please do this in a community forum or repository used and known by the wider community of that language, not the Linguist repository.

Once you've received consensus that the community is happy with your proposed color change, please feel free to open a PR making the change and link to the public discussion where this was agreed by the community.
If there are official branding guidelines to support the colour choice, please link to those too.

## Testing

You can run the tests locally with:

```bash
bundle exec rake test
```

You can test the classifier locally with:

```bash
bundle exec script/cross-validation --test
```

Sometimes getting the tests running can be too much work, especially if you don't have much Ruby experience.
It's okay: be lazy and let [GitHub Actions](https://github.com/features/actions) run the tests for you.
Just open a pull request and the bot will start cranking away.

Here's our current build status: [![Actions Status](https://github.com/github/linguist/workflows/Run%20Tests/badge.svg)](https://github.com/github/linguist/actions)


## Maintainers

Linguist is maintained with :heart: by:

- **@Alhadis**
- **@lildude** (GitHub staff)

As Linguist is a production dependency for GitHub we have a couple of workflow restrictions:

- Anyone with commit rights can merge Pull Requests provided that there is a :+1: from a GitHub staff member.
- Releases are performed by GitHub staff so we can ensure GitHub.com always stays up to date with the latest release of Linguist and there are no regressions in production.

[grammars]: /vendor/README.md
[heuristic]: https://github.com/github/linguist/blob/master/lib/linguist/heuristics.yml
[languages]: /lib/linguist/languages.yml
[licenses]: https://github.com/github/linguist/blob/9b1023ed5d308cb3363a882531dea1e272b59977/vendor/licenses/config.yml#L4-L15
[samples]: /samples
[search-example]: https://github.com/search?type=code&q=NOT+is%3Afork+path%3A*.boot
[#5756]: https://github.com/github/linguist/issues/5756
[merged-pr]: /docs/troubleshooting.md#my-linguist-pr-has-been-merged-but-gitHub-doesnt-reflect-my-changes
