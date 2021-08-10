# Releasing

This is the procedure for making a new release of Linguist. The entire process needs to be performed by a member of GitHub staff.

1. Create a branch for the release: `git checkout -b release-vxx.xx.xx`
2. Make sure your local dependencies are up to date: `rm Gemfile.lock && script/bootstrap`
3. If the grammar submodules have not been updated recently, update them: `git submodule update --remote`.
   If any submodules are updated:
    1. update the `grammars.yml`: `script/grammar-compiler update -f`
    2. confirm the updated grammars still compile and no new errors have been introduced and none have gone missing: `bundle exec rake check_grammars`
    3. verify and fix any problems identified above
    4. commit all changes: `git commit -a`
    5. update the license cache: `bundle exec licensed cache -c vendor/licenses/config.yml`
    6. double check no license problems found: `bundle exec licensed status -c vendor/licenses/config.yml`
    7. verify and fix any problems identified above
    8. commit all changes: `git commit -a`
4. Ensure that samples are updated: `bundle exec rake samples`
5. Ensure that tests are green: `bundle exec rake test`
6. Build a test gem `GEM_VERSION=$(git describe --tags 2>/dev/null | sed 's/-/./g' | sed 's/v//') bundle exec rake build_gem`
7. Test the test gem:
   1. Bump the Gemfile and Gemfile.lock versions for an app which relies on this gem
   2. Install the new gem locally
   3. Test behavior locally, branch deploy, whatever needs to happen
8. Bump gem version in `lib/linguist/VERSION`, [like this](https://github.com/github/linguist/commit/3212355400974ce5f7873a71eb8b85b1c5f4a6d2).
9. Make a PR to `github/linguist`, [like this](https://github.com/github/linguist/pull/5084).
10. Build a local gem: `bundle exec rake build_gem`
11. Merge the `github/linguist` PR
12. Tag and push: `git tag vx.xx.xx; git push --tags`
13. Create a GitHub release with the pushed tag (https://github.com/github/linguist/releases/new) and populate it with a list of the commits from `git log --pretty=format:"- %s" --reverse refs/tags/[OLD TAG]...refs/tags/[NEW TAG]` [like this](https://github.com/github/linguist/releases/tag/v7.2.0)
14. Build a grammars tarball (`./script/build-grammars-tarball`) and attach it to the GitHub release
15. Push to rubygems.pkg.github.com -- `gem push --key github --host https://rubygems.pkg.github.com/github github-linguist-3.0.0.gem`. See [Configuring RubyGems for use with GitHub Package Registry][gpr] for more details.
16. Push to rubygems.org -- `gem push github-linguist-3.0.0.gem`
17. Update and deploy the following repositories to use the new gem in production:
    - `github/github` - label for backporting to the latest version of GitHub Enterprise Server only.
    - `github/treelights` - this only needs the Linguist version updated to pull the compiled grammars from the Linguist release. Label for backporting to the latest version of GitHub Enterprise Server only.
    - `github/lightshow`

    Note: syntax highlighting changes won't take effect until the updated `github/treelights` repo has been deployed.
