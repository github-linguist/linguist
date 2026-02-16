# Configuration root

**Key**: root  
**Default value**:

   1. the root of the local git repository, if run inside a git repository
   1. the directory that `licensed` is run from

An application's root path is used as the base for any relative configuration paths in the application.

From a configuration file, the root value can be specified as one of the following.  Path string values can contain special path characters.

- a relative path from the configuration file location
- an absolute path
- `true` to use the configuration file's directory as the root

When creating a `Licensed::AppConfiguration` manually with a `root` property, the property must be an absolute path - no path expansion will occur.

```yml
root: path/from/configuration
# or
root: /absolute/path/to/root
# or
root: ~/path/from/home/to/root
# or
root: true
```
