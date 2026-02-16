# Additional terms

The `additional_terms` configuration option is used to specify paths to files containing extra licensing terms that do not ship with the dependency package. All files specified are expected to be plain text.

Files containing additional content can be located anywhere on disk that is accessible to licensed.  File paths can be specified as a string or array and can contain glob values to simplify configuration inputs.  All file paths are evaluated from the [configuration root](./configuration_root.md).

## Examples

**Note** The examples below specify paths to additional files under the `.licenses` folder.  This is a logical place to store files containing license terms, but be careful not to store files under paths managed by licensed like `.licenses/<source type>/...`.  Running `licensed cache` in the future will delete any files under licensed managed paths that licensed did not create.  This is why the below examples use paths like `.licenses/amendments/bundler/...` instead of not `.licenses/bundler/amendments/...`.

### With a string

```yaml
additional_terms:
  # specify the type of dependency
  bundler:
    # specify the dependency name and path to an additional file
    <gem-name>: .licenses/amendments/bundler/<gem-name>/terms.txt
```

### With a glob string

```yaml
additional_terms:
  # specify the type of dependency
  bundler:
    # specify the dependency name and one or more additional files with a glob pattern
    <gem-name>: .licenses/amendments/bundler/<gem-name>/*.txt
```

### With an array of strings

```yaml
additional_terms:
  # specify the type of dependency
  bundler:
    # specify the dependency name and array of paths to additional files
    <gem-name>:
      - .licenses/amendments/bundler/<gem-name>/terms-1.txt
      - .licenses/amendments/bundler/<gem-name>/terms-2.txt
```
