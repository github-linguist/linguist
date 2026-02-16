# `licensed migrate`

Migrates the licensed configuration and cached metadata files from a previous version to the most recent version.  This is not required for all major version updates.  See [migrations documentation](../migrations) for details on the migrations needed for each major version.

## Options

- `--config`/`-c`: the path to the licensed configuration file
   - default value: `./.licensed.yml`
- `--from`/`-f`: the licensed version to migrate from
   - required
