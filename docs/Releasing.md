
# Releasing

This is the procedure for creating a new release of <br>
**Linguist** that can only be performed by **GitHub Staff**.

<br>

1.  Create a new branch for the release.

    ```shell
    git checkout -b release-vxx.xx.xx
    ```

    <br>

2.  Make sure your local dependencies are up to date.

    ```shell
    rm Gemfile.lock && script/bootstrap
    ```
    
    <br>

3.  If the grammar sub-modules have not <br>
    been updated recently, update them: 
    
    ```shell
    git submodule update --remote
    ```
    
    <br>

    If any sub-modules are updated:
    
    <br>
    
    1.  Update the `grammars.yml`
    
        ```shell
        script/grammar-compiler update -f
        ```
        
        <br>

    2.  Confirm the updated grammars still compile <br>
        and no new errors have been introduced and <br>
        none have gone missing.
    
        ```shell
        bundle exec rake check_grammars
        ```
        
        <br>

    3.  Verify and fix any problems identified above.

        <br>

    4.  Commit all changes.
    
        ```
        git commit -a
        ```
        
        <br>

    5.  Update the license cache.
    
        ```shell
        bundle exec licensed cache -c vendor/licenses/config.yml
        ```
        
        <br>

    6.  Double check no license problems are found. 
    
        ```shell
        bundle exec licensed status -c vendor/licenses/config.yml
        ```
        
        <br>

    7.  Verify and fix any problems identified above.

        <br>

    8.  Commit all changes.
    
        ```shell
        git commit -a
        ```
    
    <br>

4.  Ensure that samples are updated.

    ```shell
    bundle exec rake samples
    ```

    <br>
    
5.  Ensure that tests are green.

    ```shell
    bundle exec rake test
    ```
    
    <br>

6.  Build a test gem.

    ```shell
    GEM_VERSION=$(git describe --tags 2>/dev/null | sed 's/-/./g' | sed 's/v//') bundle exec rake build_gem
    ```
    
    <br>

7.  Test the test gem.

    1.  Bump the `Gemfile` and `Gemfile.lock` <br>
        versions for apps that rely on Linguist.

    2.  Install the new gem locally.

    3.  Test the behavior locally, branch deploy, whatever needs to happen.

    <br>

8.  Bump the gem version in `lib/linguist/VERSION`
    
    *[» Check out how to.][Bump Version]*

    <br>

9.  Make a pull request to `github/linguist`.
    
    *[» Check out how to.][Pull Request]*
    
    <br>

10. Build a local gem
    
    ```shell
    bundle exec rake build_gem
    ```
    
    <br>

11. Merge the `github/linguist` pull request.

    <br>

12. Tag and push

    ```shell
    git tag vx.xx.xx; git push --tags
    ```
    
    <br>

13. Create a **[GitHub Release]** with the pushed tag <br>
    and populate it with a list of the commits from

    ```shell
    git log --pretty=format:"- %s" --reverse refs/tags/[OLD TAG]...refs/tags/[NEW TAG]
    ```
    
    *[» Check out how to.][Creating Release]*
     
    <br>

14. Build a grammars tarball and attach it to the GitHub release.

    ```shell
    ./script/build-grammars-tarball
    ```
    
    <br>

15. Push to `rubygems.pkg.github.com`

    ```shell
    gem push --key github --host https://rubygems.pkg.github.com/github github-linguist-3.0.0.gem
    ```
    
    *[» Check out how to work with the RubyGems registry.][Ruby Registry]*
    
    <br>

16. Push to `rubygems.org`

    ```shell
    gem push github-linguist-3.0.0.gem
    ```

    <br>

17. Update & deploy the following repositories <br>
    to use the new gem in production:

    -   `github/github`
    
        Label for back-porting to the latest version of GitHub Enterprise Server only.

    -   `github/treelights`
        
        This only needs the Linguist version updated to pull <br>
        the compiled grammars from the Linguist release.
        
        Label for back-porting to the latest version of GitHub Enterprise Server only.

    <br>

    **Note:** Syntax highlighting changes won't take effect until <br>
    the updated `github/treelights` repo has been deployed.

<br>


<!----------------------------------------------------------------------------->

[Creating Release]: https://github.com/github/linguist/releases/tag/v7.2.0
[GitHub Release]: https://github.com/github/linguist/releases/new
[Ruby Registry]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-rubygems-registry
[Bump Version]: https://github.com/github/linguist/commit/3212355400974ce5f7873a71eb8b85b1c5f4a6d2
[Pull Request]: https://github.com/github/linguist/pull/5084
