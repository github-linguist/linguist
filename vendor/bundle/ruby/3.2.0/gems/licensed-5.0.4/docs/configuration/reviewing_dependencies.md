# Reviewing dependencies

**Key**: reviewed
**Default value**: none

Sometimes your projects will use a dependency with an OSS license that you don't want to globally allow but can use with individual review.
The list of reviewed dependencies is meant to cover this scenario and will prevent the status command from raising an error for
a dependency with a license not on the allowed list.

The reviewed dependency list is organized based on the dependency source type - `bundler`, `go`, etc.  Add a dependency's metadata identifier to the appropriate source type sub-property to cause `licensed` to ignore license compliance failures.  Glob patterns can be used to identify multiple internal dependencies without having to manage a large list.

_NOTE: marking a dependency as reviewed will not prevent licensed from raising an error on missing license information._

```yml
reviewed:
  bundler:
    - gem-using-unallowed-license
```

## Reviewing dependencies at specific versions

Review a dependency at specific versions by appending `@<version>` to the end of the dependency's name in an `reviewed` list.  If a dependency is configured to be reviewed at a specific version, licensed will not recognize non-matching versions of the dependency as being manually reviewed and accepted.

The version value can be one of:

1. `"*"` - match any version value
1. any version string, or version range string, that can be parsed by `Gem::Requirement`
   - a semantic version - `dependency@1.2.3`
   - a gem requirement range - `dependency@~> 1.0.0` or `dependency@< 3.0`
   - see the [Rubygems version guides](https://guides.rubygems.org/patterns/#pessimistic-version-constraint) for more details about specifying gem version requirements
1. a value that can't be parsed by `Gem::Requirement`, which will only match dependencies with the same version string
