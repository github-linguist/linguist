## Contributing

[fork]: https://github.com/github/licensed/fork
[pr]: https://github.com/github/licensed/compare
[style]: https://github.com/styleguide/ruby
[code-of-conduct]: CODE_OF_CONDUCT.md

Hi there! We're thrilled that you'd like to contribute to this project. Your help is essential for keeping it great.

Please note that this project is released with a [Contributor Code of Conduct][code-of-conduct]. By participating in this project you agree to abide by its terms.

## Submitting a pull request

0. [Fork][fork] and clone the repository
0. Configure and install the dependencies: `script/bootstrap`
0. Setup test fixtures: `bundle exec rake setup`
0. Make sure the tests pass on your machine: `bundle exec rake test`
0. Create a new branch: `git checkout -b my-branch-name`
0. Make your change, add tests, and make sure the tests still pass
0. Push to your fork and [submit a pull request][pr]
0. Pat your self on the back and wait for your pull request to be reviewed and merged.

Here are a few things you can do that will increase the likelihood of your pull request being accepted:

- Follow the [style guide][style].
- Write tests.
- Keep your change as focused as possible. If there are multiple changes you would like to make that are not dependent upon each other, consider submitting them as separate pull requests.
- Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

#### Adding a new Dependency Source

Pull requests that include a new dependency source must also

- Include [documentation](docs/sources) for the new source and update the [documented source list](README.md#sources).
- Add a [setup script](script/source-setup) if needed.
- Include [tests](test/source) and [test fixtures](test/fixtures) needed to verify the source in CI.
- Add a CI job to [.github/workflows/test.yml](.github/workflows/test.yml).

## Releasing
If you are the current maintainer of this gem:

1. Create a branch for the release: git checkout -b cut-release-xx.xx.xx
2. Make sure your local dependencies are up to date: `script/bootstrap`
3. Ensure that tests are green: `bundle exec rake test`
4. Bump gem version in lib/licensed/version.rb.
5. Update [`CHANGELOG.md`](CHANGELOG.md)
6. Make a PR to github/licensed.
7. Build a local gem: bundle exec rake build
8. Test the gem:
   1. Bump the Gemfile and Gemfile.lock versions for an app which relies on this gem
   2. Install the new gem locally
   3. Test behavior locally, branch deploy, whatever needs to happen
9. Merge github/licensed PR
10. Create a new [github/licensed release](https://github.com/github/licensed/releases)
   - Set the release name and tag to the release version - `x.xx.x`
   - Set the release body to the changelog entries for the release

The following steps will happen automatically from a GitHub Actions workflow
after creating the release. In case that fails, the following steps can be performed manually

11. Push the gem from (7) to rubygems.org -- `gem push licensed-x.xx.xx.gem`

## Resources

- [How to Contribute to Open Source](https://opensource.guide/how-to-contribute/)
- [Using Pull Requests](https://help.github.com/articles/about-pull-requests/)
- [GitHub Help](https://help.github.com)
