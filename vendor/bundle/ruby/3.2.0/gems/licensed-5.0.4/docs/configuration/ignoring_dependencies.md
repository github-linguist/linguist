# Ignoring dependencies

**Key**: ignored
**Default value**: none

This configuration property is used to fully ignore a dependency during all `licensed` commands.  Any dependency on this list will not
be enumerated, or have its metadata cached or checked for compliance.  This is intended for dependencies that do not require attribution
or compliance checking - internal or 1st party dependencies, or dependencies that do not ship with the product such as test frameworks.

The ignored dependency list is organized based on the dependency source type - `bundler`, `go`, etc.  Add a dependency's metadata identifier to the appropriate source type sub-property to cause `licensed` to no longer take action on the dependency.  Glob patterns can be used to identify multiple internal dependencies without having to manage a large list.

```yml
ignored:
  bundler:
    - my-internal-gem
    - my-first-party-gem
  go:
    - github.com/me/my-repo/**/*
```

## Ignoring dependencies at specific versions

Ignore a dependency at specific versions by appending `@<version>` to the end of the dependency's name in an `ignore` list.  If a dependency is configured to be ignored at a specific version, licensed will not ignore non-matching versions of the dependency.

The version value can be one of:

1. `"*"` - match any version value
1. any version string, or version range string, that can be parsed by `Gem::Requirement`
   - a semantic version - `dependency@1.2.3`
   - a gem requirement range - `dependency@~> 1.0.0` or `dependency@< 3.0`
   - see the [Rubygems version guides](https://guides.rubygems.org/patterns/#pessimistic-version-constraint) for more details about specifying gem version requirements
1. a value that can't be parsed by `Gem::Requirement`, which will only match dependencies with the same version string
