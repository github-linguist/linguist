# Commands

Run `licensed -h` to see help content for running licensed commands.

- [cache](cache.md)
- [env](env.md)
- [list](list.md)
- [migrate](migrate.md)
- [notices](notices.md)
- [status](status.md)
- [version](version.md)

Most commands accept a `-c`/`--config` option to specify a path to a configuration file or directory. If a directory is specified, `licensed` will look in that directory for a file named (in order of preference):

1. `.licensed.yml`
2. `.licensed.yaml`
3. `.licensed.json`

If the option is not specified, the value will be set to the current directory.

## Adding a new command

### Implement new `Command` class

Licensed commands inherit and override the [`Licensed::Sources::Command`](../lib/licensed/commands/command.rb) class.

### Required method overrides

1. `Licensed::Commands::Command#evaluate_dependency`
   - Runs a command execution on an application dependency.

The `evaluate_dependency` method should contain the specific command logic.  This method has access to the application configuration, dependency source enumerator and dependency currently being evaluated as well as a reporting hash to contain information about the command execution.

### Optional method overrides

The following methods break apart the different levels of command execution.  Each method wraps lower levels of command execution in a corresponding reporter method.

1. `Licensed::Commands::Command#run`
   - Runs `run_app` for each application configuration found.  Wraps the execution of all applications in `Reporter#report_run`.
2. `Licensed::Commands::Command#run_app`
   - Runs `run_source` for each dependency source enumerator enabled for the application configuration.  Wraps the execution of all sources in `Reporter#report_app`.
3. `Licensed::Commands::Command#run_source`
   - Runs `run_dependency` for each dependency found in the source.  Wraps the execution of all dependencies in `Reporter#report_source`.
4. `Licensed::Commands::Command#run_dependency`
   - Runs `evaluate_dependency` for the dependency.  Wraps the execution of all dependencies in `Reporter#report_dependency`.

As an example, `Licensed::Commands::Command#run_app` calls `Reporter#report_app` to wrap every call to `Licensed::Commands::Command#run_source`.

### Specifying additional report data

The `run` methods can be overridden and pass a block to `super` to provide additional reporting data or functionality.

```ruby
def run_app(app)
  super do |report|
    report["my_app_data"] = true
  end
end
```
