# Yarn

The yarn source will detect dependencies when `package.json` and `yarn.lock` are found at an app's `source_path`.

It uses the `yarn` CLI commands to enumerate dependencies and gather metadata on each package.

## Including development dependencies

**Note** Yarn versions < 1.3.0 will always include non-production dependencies due to a bug in those versions of yarn.
**Note** Yarn versions > 2.0 will always include non-production dependencies due to lack of filtering of production vs non-production dependencies in the yarn CLI.

For yarn versions between 1.3.0 and 2.0, the yarn source excludes non-production dependencies by default.  To include development and test dependencies in these versions, set `production_only: false` in `.licensed.yml`.

```yml
yarn:
  production_only: false
```
