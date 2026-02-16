# Application name

**Key**: name  
**Default value**: The directory name of the application's source path

The name of the application is primarily used for organizational and display purposes.  Application names are not included with
dependency metadata information included in cached files.

```yml
source_path: path/to/application1
name: application1
```

## Dynamically generated application names

### Source path directory name

When not specified, an application's name will be the directory name from the application's source path.

```yml
# if not explicitly set the name will be inferred from the source path
source_path: path/to/application1
# name: application1

# or, use the `directory_name` name generator to explicitly set this behavior
source_path: path/to/application1
name:
  generator: directory_name
# name: application1
```

### Relative path from configuration root

Application names can be created from the path from the configuration root to the application source path.
This can be useful when specifying multiple applications using a glob pattern in a directory structure where directory names
are not unique.

As an example, given the following directory structure and configuration YML, the resulting application names
would be `linux.installer`, `windows.installer` and `mac.installer`.  The optional arguments `separator` and `depth` are used
to better control the resulting application name.

```text
path
|_to
  |_linux
    |_installer
  |_windows
    |_installer
  |_mac
    |_installer
```

```yml
source_path: '**/installer'
name:
  generator: relative_path
  # separator controls what character separates path parts in the application name
  # default: "-"
  separator: '.'
  # depth controls how many of the path parts are used in the application name
  # default: 0 / all path parts
  depth: 2
```
