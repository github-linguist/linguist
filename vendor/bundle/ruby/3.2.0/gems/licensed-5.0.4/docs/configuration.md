# Configuration file

A configuration file specifies the details of enumerating and operating on license metadata for apps.

Configuration can be specified in either YML or JSON formats, with examples given in YML.  The example
below describes common configuration values and their purposes.  See [configuration options documentation](./configuration)
for in depth information.

Additionally, some dependency sources have their own specific configuration options.  See the [source documentation](./sources) for details.

```yml
# If not set, defaults to the directory name of `source_path`
name: 'My application'

# Path is relative to the location of the configuration file and specifies
# the root to expand all paths from
# If not set, defaults to a git repository root
root: 'relative/path/from/configuration/file/directory'

# Path is relative to configuration root and specifies where cached metadata will be stored.
# If not set, defaults to '.licenses'
cache_path: 'relative/path/to/cache'

# Path is relative to configuration root and specifies the working directory when enumerating dependencies
# Optional for single app configuration, required when specifying multiple apps
# Defaults to current directory when running `licensed`
source_path: 'relative/path/to/source'

# Whether to take any action when records are detected in the cache paths that don't map to evaluated
# dependencies.
# Available values are:
# - 'error': treat stale cached records as errors.  Notify the user and fail status checks
# - 'warn', '', unset: treat stale cached records as warnings.  Notify the user but do not fail status checks
# - 'ignore': Ignore stale cached records.  Do not notify the user and do not fail status checks
# Optional, when not set this defaults to 'warn' behavior
stale_records_action: 'warn'

# Sources of metadata
sources:
  bower: true
  bundler: false

# Dependencies with these licenses are allowed and will not raise errors or warnings.
# This list does not have a default value and is required for `licensed status`
# to succeed.
allowed:
  - mit
  - apache-2.0
  - bsd-2-clause
  - bsd-3-clause
  - cc0-1.0
  - isc

# These dependencies are ignored during enumeration.
# They will not be cached, and will not raise errors or warnings.
# This configuration is intended to be used for dependencies that don't need to
# be included for compliance purposes, such as other projects owned by the current
# project's owner, internal dependencies, and dependencies that aren't shipped with
# the project like test frameworks.
ignored:
  bundler:
    - some-internal-gem

  bower:
    - some-internal-package

# These dependencies have licenses not on the `allowed` list and have been reviewed.
# They will be cached and checked, but will not raise errors or warnings for a
# non-allowed license.  Dependencies on this list will still raise errors if
# license text cannot be found for the dependency.
reviewed:
  bundler:
    - bcrypt-ruby

  bower:
  - classlist # public domain
  - octicons

# Specify additional license terms that have been obtained from a dependency's owner
# which apply to the dependency's license 
additional_terms:
  bundler:
    bcrypt-ruby:
      - .licenses/amendments/bundler/bcrypt-ruby/amendment.txt

# A single configuration file can be used to enumerate dependencies for multiple
# projects.  Each configuration is referred to as an "application" and must include
# a source path, at a minimum
apps:
  - source_path: path/to/application1
  - source_path: path/to/application2
```
