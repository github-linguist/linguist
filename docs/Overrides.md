
# Overrides

Linguist supports a number of different override <br>
strategies for language definitions and file paths.

<br>

## Git Attributes

Add a  [`.gitattributes`]  file to your project and use standard <br>
git-style path matchers for the files you want to override using <br>
the attributes described below.

When testing with a local installation of Linguist, **take note that the <br>
added attributes will not take effect until the  ` .gitattributes ` <br>
file is committed to your repository.**

*File and folder paths inside are calculated relative* <br>
*to the position of the `.gitattributes` file.*

<br>

```gitattributes
# Example of a `.gitattributes` file which reclassifies `.rb` files as Java:
*.rb linguist-language=Java

# Replace any whitespace in the language name with hyphens:
*.glyphs linguist-language=OpenStep-Property-List
```

<br>

| Attribute | Defined In | Effect 
|:----------|:----------:|:-------
| `linguist-detectable`     | [`languages.yml`]     | Included in stats, even if language's type is `data` or `prose`
| `linguist-documentation`  | [`documentation.yml`] | Excluded from stats
| `linguist-generated`      | [`generated.rb`]      | Excluded from stats, hidden in diffs
| `linguist-language=name`  | [`languages.yml`]     | Highlighted and classified as `name`
| `linguist-vendored`       | [`vendor.yml`]        | Excluded from stats

<br>

### Detectable

By default only languages of type `programming` or `markup` <br>
in [`languages.yml`] are included in the language statistics.

Languages of other types are not 'detectable' by default, <br>
causing them not to be included in the language statistics, <br>
but can be made detectable as shown below.

Languages that are not yet mentioned in [`languages.yml`] <br>
will not be included in the language statistics, even if you <br>
specify something like the following in the attributes file.

```gitattributes
*.mycola linguist-language=MyCoolLang linguist-detectable
```

Use the `linguist-detectable` attribute <br>
to mark or unmark paths as detectable:

```gitattributes
*.kicad_pcb linguist-detectable
*.sch linguist-detectable
tools/export_bom.py -linguist-detectable
```

<br>

### Documentation

Linguist excludes documentation files <br>
from your project's language statistics.

[`documentation.yml`] lists common documentation paths and <br>
excludes them from the language statistics for your repository.

Use the `linguist-documentation` attribute <br>
to mark or unmark paths as documentation:

```gitattributes
# Apply override to all files in the directory
project-docs/* linguist-documentation
# Apply override to a specific file
docs/formatter.rb -linguist-documentation
# Apply override to all files and directories in the directory
ano-dir/** linguist-documentation
```

<br>

### Generated Code

Not all plain text files are true source files.

Generated files like minified JavaScript and <br>
compiled CoffeeScript can be detected and <br>
excluded from language stats.

Unlike vendored / documentation files, these <br>
files are addionally suppressed in diffs.

The [`generated.rb`] script lists common generated paths and <br>
excludes them from the language statistics of your repository.

Use the `linguist-generated` attribute <br>
to mark or unmark paths as generated.

```gitattributes
Api.elm linguist-generated
```

<br>

### Vendored Code

Checking code you didn't write, such as JavaScript libraries, into <br>
your git repo is a common practice, but this often inflates your <br>
project's language stats and may even cause your project to be <br>
labeled as another language.

By default, Linguist treats all of the paths defined in [`vendor.yml`] <br>
as vendored and therefore doesn't include them in the language <br>
statistics for a repository.

Use the `linguist-vendored` attribute to mark paths.

```gitattributes
# Apply override to all files in the directory
special-vendored-path/* linguist-vendored
# Apply override to a specific file
jquery.js -linguist-vendored
# Apply override to all files and directories in the directory
ano-dir/** linguist-vendored
```

<br>
<br>

## Emacs / Vim Modelines

If you do not want to use a `.gitattributes` file to override <br>
the syntax highlighting used on GitHub.com, you can use Vim <br>
or Emacs style modelines to set the language for a single file.

Modelines can be placed anywhere within <br>
a file and are respected when determining <br>
how to syntax-highlight a file on GitHub.

<br>

### Vim

```vim
# Some examples of various styles:
vim: syntax=java
vim: set syntax=ruby:
vim: set filetype=prolog:
vim: set ft=cpp:
```

<br>

### Emacs

```
-*- mode: php; -*-
-*- c++ -*-
```

<br>

<!----------------------------------------------------------------------------->

[`.gitattributes`]: https://git-scm.com/docs/gitattributes

[`documentation.yml`]: ../lib/linguist/documentation.yml
[`languages.yml`]: ../lib/linguist/languages.yml
[`generated.rb`]: ../lib/linguist/generated.rb
[`vendor.yml`]: ../lib/linguist/vendor.yml
