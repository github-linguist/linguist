# Dependency metadata cache

**Key**: cache_path  
**Default value**: .licenses

The cache path is the root directory where `licensed` will write cached metadata files for each dependency.
By default, files will be written to a folder under the cache path in a structure like:

```text
<cache path>
|_<source name>
  |_<dependency identifier>.dep.yml
```

Cache paths can be given as an absolute or relative path and can contain special path characters

```yml
cache_path: relative/path/to/cache
# or
cache_path: /absolute/path/to/cache
# or
cache_path: ~/path/from/home/to/cache
```

## Configuring caches for multiple applications

When multiple applications are specified in a configuration file caches can be shared, inherited and explicitly configured.
Unless otherwise specified, preference is given based on the locality and intention of the configuration options.

1. explicitly configured cache paths are preferred to inherited or shared cache paths
2. shared cache paths are preferred to inherited cache paths
3. cache paths that are not otherwise set by an application will be inherited from the root configuration

### Explicit cache usage for multiple applications

Individual applications in a multi-application configuration can explicitly set cache paths.

```yml
apps:
  - source_path: path/to/application1
    cache_path: path/to/application1/.licenses
  - source_path: path/to/application2
    cache_path: path/to/application2/.licenses
```

### Sharing a single cache for multiple applications

Sharing a cache across multiple applications is possible by setting `cache_path: <path>` and `shared_cache: true` at the root level.
Individual applications can opt out of sharing a cache by explicitly setting a cache path.

```yml
shared_cache: true
cache_path: .cache/shared
apps:
  # application1 and application2 will share a cache at .cache/shared
  - source_path: path/to/application1
  - source_path: path/to/application2
  # application3 will use a separate cache at .cache/application3
  - source_path: path/to/application3
    cache_path: .cache/application3
```

This is equivalent to explicitly configuring the same cache for many applications

```yaml
apps:
  - source_path: "path/to/application1"
    cache_path: ".cache/shared"
  - source_path: "path/to/application2"
    cache_path: ".cache/shared"
```

Using the `shared_cache` key is primarily useful when specifying `source_path` as a glob pattern.

```yml
shared_cache: true
cache_path: .cache/shared
source_path: path/to/*
```

### Inheriting cache usage from the root configuration

When not otherwise specified, applications will inherit a cache path from the root configuration.
If the root configuration does not explicitly set a cache path value, the default cache path value is used.

Inherited cache paths are treated as the root location for each application's metadata cache.  Each application
will store metadata in a named subdirectory of the root location to avoid file path clashes between
applications.

```yml
# optional.  if not specified, the default value `.licenses` will be used
cache_path: .cache
apps:
  - source_path: path/to/application1
  - source_path: path/to/application2
```

```text
.cache
|_application1
  |_<source type>
    |_<dependency identifier>.dep.yml
|_application2
  |_<source type>
    |_<dependency identifier>.dep.yml
```
