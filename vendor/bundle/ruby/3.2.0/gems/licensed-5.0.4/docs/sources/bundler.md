# Bundler

The bundler source will detect dependencies `Gemfile` and `Gemfile.lock` files are found at an apps `source_path`.  The source uses the `Bundler` API to enumerate dependencies from `Gemfile` and `Gemfile.lock`.

### Excluding gem groups

The bundler source determines which gem groups to include or exclude with the following logic, in order of precedence.
1. Include all groups specified in the Gemfile
2. Exclude all groups from the `without` bundler configuration (e.g. `.bundle/config`)
3. Include all groups from the `with` bundler configuration (e.g. `.bundle/config`)
4. Exclude all groups from the `without` licensed configuration (`:development` and `:test` if not otherwise specified)

`licensed` can be configured to override the default "without" development and test groups in the configuration file.

Strings and string arrays are both :+1:

```yml
bundler:
  without: development
```

or

```yml
bundler:
  without:
    - build
    - development
    - test
```
