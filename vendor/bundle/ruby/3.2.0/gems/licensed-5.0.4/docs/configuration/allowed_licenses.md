# Allowed licenses

**Key**: allowed
**Default Value**: none

The list of allowed licenses is used with the [status command](../commands/status.md) to detail which licenses are allowable for use in the current project and do not need further review.  If a dependency uses a license that is not included in the allowed list, and the dependency is not on the ignored or reviewed dependency lists, it will be flagged and the status command will fail.

This configuration value accepts an array of lower-cased [open source license SPDX identifiers](https://spdx.org/licenses/).

```yml
# accepts lowercase SPDX license identifiers
allowed:
  - mit
  - bsd-2-clause
  - bsd-3-clause
  - isc
```
