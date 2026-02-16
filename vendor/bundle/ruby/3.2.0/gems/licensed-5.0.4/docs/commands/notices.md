# `licensed notices`

Outputs license and notice text for all dependencies in each app into a `NOTICE` file in the app's `cache_path`.  If an app uses a shared cache path, the file name will contain the app name as well, e.g. `NOTICE.my_app`.

`NOTICE` file contents are retrieved from cached records when the `--computed`/`-l` option is not set, with the assumption that cached records have already been reviewed in a compliance workflow.  When the `--computed`/`-l` option is set and a dependency's license is not found, that dependency's license text will be empty in the `NOTICE` file.

## Options

- `--config`/`-c`: the path to the licensed configuration file
   - default value: `./.licensed.yml`
- `--sources`/`-s`: runtime filter on which dependency sources are run.  Sources must also be enabled in the licensed configuration file.
   - default value: not set, all configured sources
- `--computed`/`-l`: use live computed when generating a `NOTICE` file
   - default value: not set, `NOTICE` file generated from cached records
