Linguist
========

We use this library on GitHub to detect file language types for syntax highlighting, ignore binary files, suppress generated code in diffs and generate language breakdown graphs.

Features
--------

### Language detection

* Common extensions
* Shebang
* C header files *(C/C++/Obj-C)*

### Binary detection

### Generated file detection

* Generated JS files *(minified js, compiled CoffeeScript)*
* Generated config files *(Xcode project files and nibs)*

### Ignore vendored libs

* Ignores common vendored libs conventions *(deps/, vendor/, jquery, prototype)*

### Stats

* Generates project LOC stats

Usage
-----

    file = Linguist::SourceFile.new("linguist.rb")
    file.language.name #=> "Ruby"

    file = Linguist::SourceFile.new("linguist.gem")
    file.binary? #=> true

    project = Linguist::Project.new(".")
    project.language.name  #=> "Ruby"
    project.language_stats #=> { "Ruby" => 0.98, "Shell" => 0.02 }

    # Using Grit backend
    repo = Grit::Repo.new("./.git")
    project = Linguist::Project.new(repo)


Contributing
------------

Once you've made your great commits:

1. Fork it.
2. Create a branch (`git checkout -b detect-foo-language`)
3. Commit your changes (`git commit -am "Added detection for the new Foo language"`)
4. Push to the branch (`git push origin detect-foo-language`)
5. Create a [Pull Request](http://help.github.com/pull-requests/) from your branch.
6. Promote it. Mention a public repository to demostration the value of your changes. Get others to drop in and :+1: it.
