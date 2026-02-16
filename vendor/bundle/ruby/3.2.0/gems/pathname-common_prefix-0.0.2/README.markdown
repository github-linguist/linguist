Pathname common_prefix
======================

This file provides `Pathname.common_prefix` and `Pathname#common_prefix`
which calculate the common prefix in the passed paths.

Installation
------------

    gem install pathname-common_prefix

or

    git clone git@github.com:KitaitiMakoto/pathname-common_prefix.git
    cd pathname-common_prefix
    ruby setup.rb

Usage
-----

### Multiple pathnames

    require 'pathname/common_prefix'
    
    paths = %w[
      /full/path/to/somewhere
      /full/path/to/anywhere
      /full/path/to/nowhere
      /full/path/to/somewhere/else
    ].map {|path| Pathname(path)}
    
    Pathname.common_prefix(*paths) # => #<Pathname:/full/path/to>

### Absolute and relative paths

    require 'pathname/common_prefix'
    
    Pathname.common_prefix(Pathname('/absolute/path'), Pathname('relative/path')) # => nil

### String arguments

    require 'pathname/common_prefix'
    
    paths = %w[
      path/to/somewhere
      path/to/anywhere
    ]
    
    Pathname.common_prefix(*paths) # => #<Pathname:path/to>
    

### Instance method

    require 'pathname/common_prefix'
    
    base = Pathname('/path/to/base/file')
    other = Pathname('/path/to/other/file')
    base.common_prefix(other) # => <Pathname:/path/to>
    
    another = Pathname('/path/to-another/file')
    base.common_prefix(other, another) # => <Pathname:/path>

### Command-line tool

Via pipe(standard input):

    $ tree book/
    book/
    ├── contents
    │   ├── css
    │   │   ├── 004.css
    │   │   └── common.css
    │   ├── html
    │   │   ├── 001.html
    │   │   ├── 002.html
    │   │   ├── 003.html
    │   │   ├── 004.html
    │   │   ├── 005.html
    │   │   └── 006.html
    │   └── images
    │        └── cover.png
    └── contents.opf
    
    4 directories, 10 files
    $ find book -name '*.html' | common-prefix
    book/contents/html

From file:

    $ cat paths
    /path/to/somewhere
    /path/to/anywhere
    /path/to/nowhere
    $ common-prefix paths
    /path/to

Contributors
------------

* [@kachick](https://github.com/kachick)

License
-------

This program is distributed under the Ruby's license.
But `setup.rb` is distributed under the GNU LGPL license. See the file for more information.
