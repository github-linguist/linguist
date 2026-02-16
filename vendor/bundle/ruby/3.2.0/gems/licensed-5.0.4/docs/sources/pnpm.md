# pnpm

The npm source will detect dependencies when `pnpm-lock.yaml` is found at an apps `source_path`.  It uses `pnpm licenses list` to enumerate dependencies and metadata.

All dependencies enumerated by the pnpm source include the dependency version in the dependency's name identifier.  All [reviewed](../configuration/reviewing_dependencies.md) or [ignored](../configuration/ignoring_dependencies.md) dependencies must include a version signifier in the configured dependency name.

**NOTE** [pnpm licenses list](https://pnpm.io/cli/licenses) is an experimental CLI command and subject to change.  If changes to pnpm result in unexpected or broken behavior in licensed please open an [issue](https://github.com/github/licensed/issues/new).

## Including development dependencies

By default, the npm source will exclude all development dependencies. To include development or test dependencies, set `production_only: false` in the licensed configuration.

```yml
pnpm:
  production_only: false
```

## Using licensed with pnpm workspaces

Licensed will locate all dependencies from all pnpm workspaces and cannot enumerate dependencies from individual project workspaces.  This is a limitation from the pnpm CLI.
