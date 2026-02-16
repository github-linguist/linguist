# Application source path

**Key**: source_path  
**Default value**:

- if the `apps` key is not present, then the current working directory where `licensed` was executed
- if the `apps` key is present, then `nil`

The source path is the directory in which licensed should run to enumerate dependencies.  This is often dependent
on the project type, for example the bundler source should be run from the directory containing a `Gemfile` or `gems.rb`
while the go source should be run from the directory containing an entrypoint function.

The source path is required to run `licensed`.  A default value is available only when the configuration file specifies a single application.
When multiple applications are configured, each application must specify a source path.

Paths can be given as absolute or relative paths, and can use special path identifiers. If a relative path is given, it will be based on the application's root path.

```yml
# when apps is not set, a source path does not need to be specified.  it will default to the users current directory
sources:
  bundler: true

# ------
# or a path can be given as either an absolute or relative path
sources:
  bundler: true
source_path: path/to/application1

# ------
# when apps is set, each application must specify a source_path
sources:
  bundler: true
apps:
  - source_path: relative/path/to/application1
  - source_path: /absolute/path/to/application2
  - source_path: ~/path/from/home/to/application3
```

## Expanding source paths with glob patterns

The `source_path` property can use one or more glob patterns to share configuration properties across multiple application entrypoints.

For example, there is a common pattern in Go projects to include multiple executable entrypoints under folders in `cmd`.  Using a glob pattern allows users to avoid manually configuring and maintaining multiple licensed application `source_path`s.  Using a glob pattern will also ensure that any new entrypoints matching the pattern are automatically picked up by licensed commands as they are added.

```yml
sources:
  go: true

# treat all directories under `cmd` as separate apps
source_path: cmd/*
```

In order to better filter the results from glob patterns, the `source_path` property also accepts an array of
inclusion and exclusion glob patterns similar to gitignore files.  Inclusion patterns will add matching directory
paths to resulting set of source paths, while exclusion patterns will remove matching directory paths.

```yml
source_path:
  - "projects/*" # include by default all directories under "projects"
  - "!projects/*Test" # exclude all projects ending in "Test"
```

Glob patterns are syntactic sugar for, and provide the same functionality as, manually specifying multiple `source_path` values.
See the instructions on [specifying multiple apps](../configuration.md#specifying-multiple-apps) below for additional considerations when using multiple apps.
