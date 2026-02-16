# Manifests

The manifest source can be used when no package managers are available.  The manifest source will be enabled when a manifest file is found or a manifest is configured in the configuration file.

## Manifest sources

A dependency file manifest can be specified in two ways - a dependency manifest file or in the licensed configuration file as a set of patterns used to find files.

Manifests are loaded from (in order)
- A manifest file, if found
- The licensed configuration file, if a manifest file is not found and the `manifest.dependencies` configuration property exists.

### Manifest files

Manifest files are used to match source files with their corresponding packages to find package dependencies.  Manifest file paths can be specified in the app configuration with the following setting:
```yml
manifest:
  path: 'path/to/manifest.json'
```

If a manifest path is not specified for an app, the file will be looked for at the apps `<cache_path>/manifest.json`.

The manifest can be a JSON or YAML file with a single root object and properties mapping file paths to package names.
```JSON
{
  "file1": "package1",
  "path/to/file2": "package1",
  "other/file3": "package2"
}
```

File paths are relative to the git repository root.  A metadata file, `path/to/cache/manifest/<package>.txt`, will be created for each package.

**NOTE** It is the responsibility of the repository owner to maintain the manifest file.

### Configured manifest

Manifests can be specified using patterns specified in the configuration file.

Dependencies are specified at the `manifest.dependencies` configuration property and should map dependency names to one or more patterns of files matching the dependencies.

In the following example, there are two dependencies, `package` and `nested`, where `nested` is a sub-dependency in the `vendor/package` folder.  Using inclusion and exclusion patterns we can specify that files in the `vendor/package/nested` folder belong to the `nested` package, while all other files belong to `package`.

```yaml
manifest:
  dependencies:
    package:
      - "vendor/package/**/*"
      - "!vendor/package/nested/*"
    nested: "vendor/package/nested/*"
```

This demonstrates that
1. Dependencies can be mapped to a string or an array of strings
2. Inclusion and exclusion patterns apply
3. Patterns follow a glob-style format

#### Pattern format

Patterns are evaluated using [Dir.glob](https://ruby-doc.org/core/Dir.html#method-c-glob) and must follow these rules:
1. Patterns are evaluated from the project root directory
2. Patterns will only match files that are tracked by Git
3. Patterns should follow standard shell glob syntax
4. Patterns are evaluated in the order specified - order matters!
5. `!` can be appended to any pattern to indicate a negative pattern
   - Negative patterns exclude matching files from the result set
   - If a pattern should match files starting with `!`, escape the leading `!` with `\` -> `\!filename`
6. Patterns will match dotfiles

**NOTE** If the first, or only, pattern for a dependency is a negative pattern, it will not affect the set of files matched to a dependency.  An inclusion pattern should always be specified before any negative patterns for a dependency.

#### Restrictions for specifying dependency patterns via configuration

The following restrictions will raise errors if they are not met when specifying manifest dependency patterns in the configuration file

1. The dependencies key is specified but is empty
2. A file in the project is not attributed to any of the configured dependencies
   - The manifest by default will track all files in the project to ensure that license metadata updates occur when dependencies change
   - See [Globally excluding files](#globally-excluding-files) to limit the scope of files that are tracked
3. A file in the project is attributed to multiple configured dependencies
   - All files must be tracked by a single dependency

#### Globally excluding files

Tracking project files is needed to make sure that any changes to dependencies does not go unnoticed.  What about non-dependency code?

To reduce friction on changes to project code, global exclusion patterns can be added to the configuration that will cause any matching files to **NOT** be tracked as a dependency of the project.

Global exclusion patterns follow the same rules as [dependency patterns](#pattern-format) and are set on the `manifest.exclude` property.

The following example excludes all files that are not under the `vendor` folder.

```yaml
manifest:
  exclude:
    - "**/*"
    - "!vendor/**/*"
```

## Finding license content

### From common source file directories

If multiple source files map to a single package and they share a common path under the git repository root, that directory will be used to find license information, if available.

### From source file comments

When a file containing license content is not found for a group of source files,
Licensed will attempt to parse license text from source file comments.

There are some limitations on this functionality:

1. Comments MUST contain a copyright statement
2. Comments MUST be C-style multiline comments, e.g. `/* comment */`
3. Comments SHOULD contain identical indentation for each content line.

The following examples are all :+1:.  Licensed will try to preserve formatting,
however for best results comments should not mix tabs and spaces in leading whitespace.
```
/*
   <copyright statement>

   <license text>
 */

/* <copyright statement>
   <license text>
 */

/*
 * <copyright statement>
 * <license text>
 *
 * <license text>
 */
```

### From license files specified in the configuration

If all else fails, the path to a dependency's license files can be specified manually
in the configuration.  All paths should be relative to the repository root.

```yaml
manifest:
  licenses:
    package: path/to/LICENSE
```

### License content versioning

The manifest source supports multiple versioning strategies to determine if cached dependency metadata is stale.  A version strategy is chosen based on the current app configuration.

1. Git commit SHA - This strategy uses the latest Git commit SHA available for the package's import path directory as the version.  This is the default strategy used if not otherwise configured.
   - :warning: The latest Git commit won't capture any changes that are committed alongside a cached file update.  Make sure to update cached files after all other changes are committed.

   ```yaml
   version_strategy: git # or leave this key unset
   ```
2. Contents hash - This strategy uses a hash of the files in the package's import path directory as the version.
   ```yaml
   version_strategy: contents
   ```
