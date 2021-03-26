# Overrides

Linguist supports a number of different custom override strategies for language definitions and file paths.

## Using gitattributes

Add [a `.gitattributes` file](https://git-scm.com/docs/gitattributes) to your project and use standard git-style path matchers for the files you want to override using the `linguist-documentation`, `linguist-language`, `linguist-vendored`, `linguist-generated`  and `linguist-detectable` attributes.
`.gitattributes` will be used to determine language statistics and will be used to syntax highlight files.
You can also manually set syntax highlighting using [Vim or Emacs modelines](#using-emacs-or-vim-modelines).

When testing with a local installation of Linguist, take note that the added attributes will not take effect until the `.gitattributes` file is committed to your repository.

File and folder paths inside `.gitattributes` are calculated relative to the position of the `.gitattributes` file.

```gitattributes
# Example of a `.gitattributes` file which reclassifies `.rb` files as Java:
*.rb linguist-language=Java

# Replace any whitespace in the language name with hyphens:
*.glyphs linguist-language=OpenStep-Property-List
```

### Summary

<!------------------------------------------------------------------------------------------------------------------------------------------->
 | Git attribute                                  | Defined in            | Effect on file                                                  |
 |:-----------------------------------------------|:----------------------|:----------------------------------------------------------------|
 | `linguist-detectable`                          | [`languages.yml`]     | Included in stats, even if language's type is `data` or `prose` |
 | `linguist-documentation`                       | [`documentation.yml`] | Excluded from stats                                             |
 | `linguist-generated`                           | [`generated.rb`]      | Excluded from stats, hidden in diffs                            |
 | `linguist-language`=<var><ins>name</ins></var> | [`languages.yml`]     | Highlighted and classified as <var><ins>name</ins></var>        |
 | `linguist-vendored`                            | [`vendor.yml`]        | Excluded from stats                                             |
<!------------------------------------------------------------------------------------------------------------------------------------------->

### Detectable

Only programming languages are included in the language statistics.
Languages of a different type (as defined in [`languages.yml`]) are not "detectable" causing them not to be included in the language statistics.

Use the `linguist-detectable` attribute to mark or unmark paths as detectable:

```gitattributes
*.kicad_pcb linguist-detectable
*.sch linguist-detectable
tools/export_bom.py -linguist-detectable
```

### Documentation

Just like vendored files, Linguist excludes documentation files from your project's language stats.
[`documentation.yml`] lists common documentation paths and excludes them from the language statistics for your repository.

Use the `linguist-documentation` attribute to mark or unmark paths as documentation:

```gitattributes
project-docs/* linguist-documentation
docs/formatter.rb -linguist-documentation
```

### Generated code

Not all plain text files are true source files.
Generated files like minified JavaScript and compiled CoffeeScript can be detected and excluded from language stats.
As an added bonus, unlike vendored and documentation files, these files are suppressed in diffs.
[`generated.rb`] lists common generated paths and excludes them from the language statistics of your repository.

Use the `linguist-generated` attribute to mark or unmark paths as generated.

```gitattributes
Api.elm linguist-generated
```

### Vendored code

Checking code you didn't write, such as JavaScript libraries, into your git repo is a common practice, but this often inflates your project's language stats and may even cause your project to be labeled as another language.
By default, Linguist treats all of the paths defined in [`vendor.yml`] as vendored and therefore doesn't include them in the language statistics for a repository.

Use the `linguist-vendored` attribute to vendor or un-vendor paths:

```gitattributes
special-vendored-path/* linguist-vendored
jquery.js -linguist-vendored
```

## Using Emacs or Vim modelines

If you do not want to use `.gitattributes` to override the syntax highlighting used on GitHub.com, you can use Vim or Emacs style modelines to set the language for a single file.
Modelines can be placed anywhere within a file and are respected when determining how to syntax-highlight a file on GitHub.com

### Vim
```
# Some examples of various styles:
vim: syntax=java
vim: set syntax=ruby:
vim: set filetype=prolog:
vim: set ft=cpp:
```

### Emacs
```
-*- mode: php; -*-
-*- c++ -*-
```

[`documentation.yml`]: /lib/linguist/documentation.yml
[`languages.yml`]:     /lib/linguist/languages.yml
[`generated.rb`]:      /lib/linguist/generated.rb
[`vendor.yml`]:        /lib/linguist/vendor.yml
