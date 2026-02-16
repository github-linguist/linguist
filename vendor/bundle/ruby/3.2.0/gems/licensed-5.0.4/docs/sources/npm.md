# npm

The npm source will detect dependencies `package.json` is found at an apps `source_path`.  It uses `npm list` to enumerate dependencies and metadata.

## Including development dependencies

By default, the npm source will exclude all development dependencies. To include development or test dependencies, set `production_only: false` in the licensed configuration.

```yml
npm:
  production_only: false
```

## Using licensed with npm workspaces

Licensed requires npm version 8.5.0 or later to enumerate dependencies inside of npm workspaces.  For the best results, treat each workspace directory as a separate app `source_path`:

```yml
apps:
  - source_path: path/to/workspace/a
  - source_path: path/to/workspace/b
```
