# Composer

The composer source will detect dependencies when php is available, a `composer.lock` file is found at an apps `source_path`, and a composer application file is found.

It enumerates dependencies and metadata by parsing `composer.lock` files for for dependency metadata and running `php <composer application file> show --format json --path` to obtain local dependency paths on disk.

### Composer application file

The default composer application file location is `<repository root>/composer.phar`.  To specify a custom composer file location, use the `composer.application_path` configuration setting.

```yml
composer:
  application_path: "/path/to/composer"
```

### Dev dependencies

By default licensed ignores all dev dependencies. To consider dev dependencies as well, use the `composer.include_dev` configuration setting.

```yml
composer:
  include_dev: true
```
