# `licensed list`

The list command finds and prints the dependencies for all sources in all configured applications.  No additional actions are taken on dependencies.

## Options

- `--config`/`-c`: the path to the licensed configuration file
   - default value: `./.licensed.yml`
- `--sources`/`-s`: runtime filter on which dependency sources are run.  Sources must also be enabled in the licensed configuration file.
   - default value: not set, all configured sources
- `--format`/`-f`: the output format
   - default value: `yaml`
- `--licenses`/`-l`: if set, includes each dependency's detected license in the output
   - default value: not set

### Reported Data

The following data is reported for each dependency when the YAML or JSON report formats are used

- name: the licensed recognized name for the dependency including the app and source name
   - e.g. the full name for the `thor` bundler dependency used by this tool is `licensed.bundler.thor`
- version: the version of the enumerated dependency
- license: (optional) the dependency's SPDX license identifier
