# Cabal

The cabal source uses the `ghc-pkg` command to enumerate dependencies and provide metadata.  It is un-opinionated on GHC packagedb locations and requires some configuration to ensure that all packages are properly found.

The cabal source will detect dependencies when a `.cabal` file is found at an apps `source_path`.  By default, the cabal source will enumerate dependencies for all executable and library targets in a cabal file.

### Specifying which cabal file targets should enumerate dependencies
The cabal source can be configured to override which cabal file targets contain dependencies that need to be documented.

The default configuration is equivalent to:
```yml
cabal:
  cabal_file_targets:
    - executable
    - library
```

However if you only wanted to enumerate dependencies for a `my_cabal_exe` executable target, you could specify:
```yml
cabal:
  cabal_file_targets:
    - executable my_cabal_exe
```

### Specifying GHC packagedb locations through environment
You can configure the `cabal` source to use specific packagedb locations by setting the `GHC_PACKAGE_PATH` environment variable before running `licensed`.

For example, the following is a useful configuration for use with `cabal new-build`.
```bash
ghc_version="$(ghc --numeric-version)"
CABAL_STORE_PACKAGE_DB="$(cd ~/.cabal/store/ghc-$ghc_version/package.db && pwd)"
LOCAL_PACKAGE_DB="$ROOT/dist-newstyle/packagedb/ghc-$ghc_version"
GLOBAL_PACKAGE_DB="$(ghc-pkg list --global | head -n 1)"
export GHC_PACKAGE_PATH="$LOCAL_PACKAGE_DB:$CABAL_STORE_PACKAGE_DB:$GLOBAL_PACKAGE_DB:$GHC_PACKAGE_PATH"

bundle exec licensed <args>
```

### Specifying GHC packagedb locations through configuration
Alternatively, the `cabal` source can use packagedb locations set in the app configuration.  The following is an example configuration identical to the above environment configuration.

```yml
cabal:
  ghc_package_db:
    - ~/.cabal/store/ghc-<ghc_version>/package.db
    - dist-newstyle/packagedb/ghc-<ghc_version>
    - global
```

Ordering is preserved when evaluating the configuration values.  Paths are expanded from the root of the `git` repository and used as values for CLI `--package-db="<path>"` arguments.  Additionally, in all specified paths, `<ghc_version>` will be replaced with the result of `ghc --numeric-version`.

The `global` and `user` keywords are supported and will expand to the `--global` and `--user` CLI arguments, respectively.

Like most other settings, the `cabal` configuration can be specified for the root configuration, or per-app.  If specified at root, it will be inherited by all apps unless explicitly overridden.

### Stack sandboxes
The current recommended way to use `licensed` with stack is to run the `licensed` command with `stack exec`: `stack exec -- bundle exec licensed <args>`.  This will allow stack to control the GHC packagedb locations.
