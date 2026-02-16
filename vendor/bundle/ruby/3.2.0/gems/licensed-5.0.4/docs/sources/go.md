# Go

The go source uses `go` CLI commands to enumerate dependencies and properties.  It is expected that `go` projects have been built, and that `go` environment variables are properly set before running `licensed`.

#### Source paths
Source paths for go projects should point to a location that contains an entrypoint to the executable or library.

An example usage might see a configuration like:
```YAML
source_path: go/path/src/github.com/BurntSushi/toml/cmd/tomlv
```

Note that this configuration points directly to the tomlv command source, which contains `func main`.

#### GOPATH
A valid `GOPATH` is required to successfully find `go` dependencies.  If `GOPATH` is not available in the calling environment, or if a different `GOPATH` is needed than what is set in the calling environment, a value can be set in the `licensed` configuration file.

```yaml
go:
  GOPATH: ~/go
```

The setting supports absolute, relative and expandable (e.g. "~") paths.  Relative paths are considered relative to the repository root.

Non-empty `GOPATH` configuration settings will override the `GOPATH` environment variable while enumerating `go` dependencies.  The `GOPATH` environment variable is restored once dependencies have been enumerated.

#### Reviewing and ignoring all packages from a Go module

Go's package and module structure has common conventions that documentation and metadata for all packages in a module live in the module root.  In this scenario all packages share the same LICENSE information and can be reviewed or ignored at the module level rather than per-package using glob patterns.

```yaml
reviewed:
  go:
    # review all Go packages from import paths starting with github.com/external-package
    # see the `File.fnmatch?` documentation for details on how patterns are matched.
    # comparisons use the FNM_CASEFOLD and FNM_PATHNAME flags
    - github.com/external-package/**/*

ignored:
  go:
    # ignore all Go packages from import paths starting with github.com/internal-package
    # see the `File.fnmatch?` documentation for details on how patterns are matched.
    # comparisons use the FNM_CASEFOLD and FNM_PATHNAME flags
    - github.com/internal-package/**/*
```

#### Versioning

The go source supports multiple versioning strategies to determine if cached dependency metadata is stale.  A version strategy is chosen based on the availability of go module information along with the current app configuration.

1. Go Module version - This strategy uses the version of the go module.
   - :exclamation: This strategy will always be used if go module information is available because the version comes from an externally provided identifier.  Locating the version of the source package used via this identifier will be easier than other strategies.
2. Git commit SHA - This strategy uses the latest Git commit SHA available for the package's import path directory as the version.  This is the default strategy used if a go module version isn't available and the setting is not configured.
   - :warning: The latest Git commit won't capture any changes that are committed alongside a cached file update.  Make sure to update cached files after all other changes are committed.

   ```yaml
   version_strategy: git # or leave this key unset
   ```
3. Contents hash - This strategy uses a hash of the files in the package's import path directory as the version.
   ```yaml
   version_strategy: contents
   ```

#### Go modules support

The go source fully supports go modules, provided that the calling environment has been configured to use go modules.

The go source can be configured to support vendored go modules
```yaml
go:
  mod: vendor
```
