# Licensed Command Reporters

Reporters are responsible for providing feedback for a command execution.  They have
access points for each level of data encountered when executing a command.
```
command run
|
| - app
    |
    | - source
        |
        | - dependency
```

## Reporters

### Cache reporter

This reporter presents the results of running a cache command in a human-readable format.
It outputs the name for each app configuration and dependency source, as well
as showing whether a dependency was cached or if a cached record was found and reused.

example output
```
Caching dependency records for licensed
  bundler
    Caching licensee (9.11.0)
    Caching thor (0.20.3)
    Caching pathname-common_prefix (0.0.1)
    Caching tomlrb (1.2.8)
    Caching bundler (1.16.3)
    Caching dotenv (2.6.0)
    Caching octokit (4.8.0)
    Caching rugged (0.27.7)
    Caching sawyer (0.8.1)
    Caching addressable (2.5.2)
    Caching faraday (0.15.4)
    Caching public_suffix (3.0.3)
    Caching multipart-post (2.0.0)
  * 13 bundler dependencies
```

### List reporter

This reporter presents the results of running a list command in a human-readable format.
It outputs the name for each app configuration and dependency source, as well
the name and version of each dependency found.

example output
```
Listing dependencies for licensed
  bundler
    addressable (2.5.2)
    bundler (1.16.3)
    dotenv (2.6.0)
    faraday (0.15.4)
    licensee (9.11.0)
    multipart-post (2.0.0)
    octokit (4.8.0)
    pathname-common_prefix (0.0.1)
    public_suffix (3.0.3)
    rugged (0.27.7)
    sawyer (0.8.1)
    thor (0.20.3)
    tomlrb (1.2.8)
  * 13 bundler dependencies
```

### Status reporter

This reporter presents the results of running a status command in a human-readable format.
It outputs the name for each app configuration and dependency source, as well
as whether each cached dependency record has any errors using a dot format.
All errors found are displayed as well.

example output
```
Checking cached dependency records for licensed
..F.F....F...

Errors:

* bundler.pathname-common_prefix
  filename: /Users/jonabc/github/licensed/.licenses/bundler/pathname-common_prefix.dep.yml
    - license needs review: other

* bundler.bundler
  filename: /Users/jonabc/github/licensed/.licenses/bundler/bundler.dep.yml
    - cached dependency record not found  

* bundler.addressable
  filename: /Users/jonabc/github/licensed/.licenses/bundler/addressable.dep.yml
    - license needs review: apache-2.0
```

## Creating a new reporter

For examples of reporters, please see [`lib/licensed/reporters`](../lib/licensed/reporters).

All reporters should inherit from `Licensed::Reporters::Reporter` and override
each reporting method needed to return the appropriate data.

The following reporting points are available to give context or results from different scopes:
1. `#report_run` - reporting on a command execution
2. `#report_app(app)` - reporting on an application configuration
3. `#report_source(source)` - reporting on a dependency source enumerator
4. `#report_dependency(dependency)` - reporting on an individual dependency

Each method expects the caller to provide a block to execute for that reporting scope.
The block will be given a `report` hash object that the caller can use to provide any additional
details about the operations done in the reporting scope.

For example, the cache command lets the reporter know whether a dependency record
was cached or reused by setting a `cached` report value
```ruby
reporter.report_dependency(dependency) do |report|
  cached_record = Licensed::DependencyRecord.read(filename)
  report["cached"] = force || save_dependency_record?(dependency, cached_record)
  ...
end
```

When implementing each reporting method, defining a new nested block gives flexibility
to display messages both before and after the scope execution.

For example, a reporter can show header and footer content to a source scope with
```ruby
super do |report|
  shell.info "  dependencies:"
  result yield report
  shell.confirm "  * #{report.size} dependencies"
  ...
  result
end
```

Reporting methods should also return the result of the yielded block as seen above,
in order to allow transparent data returns through reporting blocks.

### Data reports

Report hashes are nested and available to higher scopes. As an example, when a
`report_source` block has finished, the source scope's `report` hash will
contain all of the dependency reports that were obtained during the `report_source` block.

Reports are keyed by:
- `app`: `app["name"]`
- `source`: `source.class.type`
- `dependency`: `dependency.name`

When a `report_run` yielded block finishes, its `report` object will contain all app reports.
Each of those app reports in turn contain each of their source's reports, and so on.
This is particularly useful if a reporter should need to review the entire execution
of a command.

For this reason though, **reporting methods MUST be used in order**.

This is valid
```ruby
report_run do
  report_app(app) do
    report_source(source) do
      report_dependency(dependency) { }
    end
  end
end
```

This is not valid
```ruby
report_run do
  report_dependency(dependency) { }
end
```
