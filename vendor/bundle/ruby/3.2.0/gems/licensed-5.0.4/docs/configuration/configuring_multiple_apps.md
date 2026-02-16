# Configuring multiple application definitions

**Key**: apps
**Required**: false

The configuration file can specify multiple source paths to enumerate metadata, each with their own configuration by using the `apps` key.
Each source path and any additional configuration make up an "application".  Root configuration settings are inherited into each application,
allowing applications to share a common configuration and reducing the overall size of the configuration file.

When the apps key is not given, the root configuration is treated as a single application.

```yml
apps:
  # application definition for "go-application"
  - source_path: path/to/go-application
    sources:
      go: true
    allowed:
      - mit

  # application definition for "ruby-application"
  - source_path: path/to/ruby-application
    sources:
      bundler: true
    allowed:
      - bsd-3-clause
```

## Inheriting configuration

Applications inherit all root configuration settings.  Inherited settings will be overridden by any configuration set directly on the application definition.

In this example, two apps have been declared.  The first app, with `source_path: path/to/application1`, inherits all configuration settings from the root configuration.  The second app, with `source_path: path/to/application2`, overrides the `sources` configuration and inherits all other settings.

```yml
sources:
  go: true
  bundler: false

ignored:
  bundler:
    - some-internal-gem

reviewed:
  bundler:
    - bcrypt-ruby

cache_path: 'path/to/cache'
apps:
  # inherits all settings from the root configuration
  - source_path: 'path/to/application1'

  # inherits all settings except for "sources" from the root configuration
  - source_path: 'path/to/application2'
    sources:
      bundler: true
      go: false
```
