# `licensed cache`

The cache command finds all dependencies and ensures that each dependency has an up-to-date cached record.

Dependency records will be saved if:

1. The `force` option is set
2. No cached record is found
3. The cached record's version is different than the current dependency's version
   - If the cached record's license text contents matches the current dependency's license text then the `license` metadata from the cached record is retained for the new saved record.

After the cache command is run, any cached records that don't match up to a current application dependency will be deleted.

## Options

- `--config`/`-c`: the path to the licensed configuration file
   - default value: `./.licensed.yml`
- `--sources`/`-s`: runtime filter on which dependency sources are run.  Sources must also be enabled in the licensed configuration file.
   - default value: not set, all configured sources
- `--format`/`-f`: the output format
   - default value: `yaml`
- `--force`: if set, forces all dependency metadata files to be recached
   - default value: not set

## Reported Data

The following data is reported for each dependency when the YAML or JSON report formats are used

- name: the licensed recognized name for the dependency including the app and source name
   - e.g. the full name for the `thor` bundler dependency used by this tool is `licensed.bundler.thor`
- cached: true when the dependency's cached metadata file was updated, false otherwise
- version: the version of the enumerated dependency
- license: the dependency's SPDX license identifier
- filename: the full path on disk to the dependency's cached metadata file, if available
- warnings: any warning messages encountered while enumerating and caching dependency metadata, if available
