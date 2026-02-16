# Rugged [![Rugged CI](https://github.com/libgit2/rugged/actions/workflows/ci.yml/badge.svg)](https://github.com/libgit2/rugged/actions/workflows/ci.yml)
**libgit2 bindings in Ruby**

Rugged is a library for accessing [libgit2](https://github.com/libgit2/libgit2) in Ruby. It gives you the speed and
portability of libgit2 with the beauty of the Ruby language.

### libgit2

libgit2 is a pure C implementation of the Git core methods. It's designed to be
fast and portable. For more information about libgit2,
[check out libgit2's website](http://libgit2.github.com) or browse the
[libgit2 organization](https://github.com/libgit2) on GitHub.

## Install

Rugged is a self-contained gem. You can install it by running:

    $ gem install rugged

## Prerequisites
You need to have CMake and `pkg-config` installed on your system to be able to build the included version of `libgit2`. 

### Debian, Including Ubuntu
All Debian-derived Linux distros provide `apt`:
```bash
$ sudo apt install libgit2-dev cmake pkg-config
```

Note that you only need `libgit2-dev` if you want to build with the system
libgit2 rather than the vendored version. In this case, note that the major and
minor versions of libgit2 and rugged must match.

### Mac
On OS X, after installing [Homebrew](http://brew.sh/), you can get the required packages with:
```bash
$ brew install cmake pkg-config
```

Please follow the above in case installation of the gem fails with `ERROR: CMake is required to build Rugged.`.

### Options

If you want to build Rugged with HTTPS and SSH support, check out the list of optional [libgit2 dependencies](https://github.com/libgit2/libgit2#optional-dependencies).

To install `rugged` with SSH support ensure you have the `LibSSH2` library present, then pass the required `CMAKE_FLAGS`:
```bash
CMAKE_FLAGS='-DUSE_SSH=ON' gem install rugged
```

Or pass the `--with-ssh` build option:
```bash
gem install rugged -- --with-ssh
```

If you're using bundler and want to bundle `libgit2` with Rugged, you can use the `:submodules` option:

```ruby
gem 'rugged', git: 'git://github.com/libgit2/rugged.git', submodules: true
```

If you would like to bundle rugged with SSH support add the `--with-ssh` build option to the bundler config:
```bash
bundle config build.rugged --with-ssh
```

## Usage
To load Rugged, you'll usually want to add something like this:

```ruby
require 'rugged'
```

### Use the system provided libgit2

By default, Rugged builds and uses a bundled version of libgit2. If you
want to use the system library instead, you can install rugged as follows:

```
gem install rugged -- --use-system-libraries
```

Or if you are using bundler:

```
bundle config build.rugged --use-system-libraries
bundle install
```

However, note that Rugged does only support specific versions of libgit2.

## Usage

Rugged gives you access to the many parts of a Git repository. You can read and
write objects, walk a tree, access the staging area, and lots more. Let's look
at each area individually.

### Repositories

#### Instantiation

The repository is naturally central to Git. Rugged has a `Repository` class that
you can instantiate with a path to open an existing repository :

```ruby
repo = Rugged::Repository.new('path/to/my/repository')
# => #<Rugged::Repository:2228536260 {path: "path/to/my/repository/.git/"}>
```

You can create a new repository with `init_at`. Add a second parameter `:bare` to make a bare repository:

```ruby
Rugged::Repository.init_at('.', :bare)
```

You can also let Rugged discover the path to the .git directory if you give it a
subdirectory.

```ruby
Rugged::Repository.discover("/Users/me/projects/repo/lib/subdir/")
# => "/Users/me/projects/repo/.git/"
```

Once your Repository instantiated (in the following examples, as `repo`), you
can access or modify it.

#### Accessing a Repository

```ruby
# Does the given SHA1 exist in this repository?
repo.exists?('07b44cbda23b726e5d54e2ef383495922c024202')
# => true

# Boolean repository state values:
repo.bare?
# => false
repo.empty?
# => true
repo.head_unborn?
# => false
repo.head_detached?
# => false

# Path accessors
repo.path
# => "path/to/my/repository/.git/"
repo.workdir
# => "path/to/my/repository/"

# The HEAD of the repository.
ref = repo.head
# => #<Rugged::Reference:2228467240 {name: "refs/heads/master", target:  #<Rugged::Commit:2228467250 {message: "helpful message", tree: #<Rugged::Tree:2228467260 {oid: 5d6f29220a0783b8085134df14ec4d960b6c3bf2}>}>

# From the returned ref, you can also access the `name`, `target`, and target SHA:
ref.name
# => "refs/heads/master"
ref.target
# => #<Rugged::Commit:2228467250 {message: "helpful message", tree: #<Rugged::Tree:2228467260 {oid: 5d6f29220a0783b8085134df14ec4d960b6c3bf2}>}>
ref.target_id
# => "2bc6a70483369f33f641ca44873497f13a15cde5"

# Reading an object
object = repo.read('a0ae5566e3c8a3bddffab21022056f0b5e03ef07')
# => #<Rugged::OdbObject:0x109a64780>
object.len
# => 237
object.data
# => "tree 76f23f186076fc291742816721ea8c3e95567241\nparent 8e3c5c52b8f29da0adc7e8be8a037cbeaea6de6b\nauthor Vicent Mart\303\255 <tanoku@gmail.com> 1333859005 +0200\ncommitter Vicent Mart\303\255 <tanoku@gmail.com> 1333859005 +0200\n\nAdd `Repository#blob_at`\n"
object.type
# => :commit
```

#### Writing to a Repository

There's a few ways to write to a repository. To write directly from your
instantiated repository object:

```ruby
sha = repo.write(content, type)
```

You can also use the `Commit` object directly to craft a commit; this is a bit
more high-level, so it may be preferable:

```ruby
oid = repo.write("This is a blob.", :blob)
index = repo.index
index.read_tree(repo.head.target.tree)
index.add(:path => "README.md", :oid => oid, :mode => 0100644)

options = {}
options[:tree] = index.write_tree(repo)

options[:author] = { :email => "testuser@github.com", :name => 'Test Author', :time => Time.now }
options[:committer] = { :email => "testuser@github.com", :name => 'Test Author', :time => Time.now }
options[:message] ||= "Making a commit via Rugged!"
options[:parents] = repo.empty? ? [] : [ repo.head.target ].compact
options[:update_ref] = 'HEAD'

Rugged::Commit.create(repo, options)
```

---

### Objects

`Object` is the main object class - it shouldn't be created directly, but all of
these methods should be useful in their derived classes.

```ruby
obj = repo.lookup(sha)
obj.oid  # object sha
obj.type # One of :commit, :tree, :blob or :tag

robj = obj.read_raw
str  = robj.data
int  = robj.len
```

There are four base object types in Git: **blobs**, **commits**, **tags**, and
**trees**. Each of these object types have a corresponding class within Rugged.

### Commit Objects

```ruby
commit = repo.lookup('a0ae5566e3c8a3bddffab21022056f0b5e03ef07')
# => #<Rugged::Commit:2245304380>

commit.message
# => "Add `Repository#blob_at`\n"

commit.time
# => Sat Apr 07 21:23:25 -0700 2012

commit.author
# => {:email=>"tanoku@gmail.com", :name=>"Vicent Mart\303\255", :time=>Sun Apr 08 04:23:25 UTC 2012}

commit.tree
# => #<Rugged::Tree:2245269740>

commit.parents
# => [#<Rugged::Commit:2245264600 {message: "Merge pull request #47 from isaac/remotes\n\nAdd Rugged::Repository#remotes", tree: #<Rugged::Tree:2245264240 {oid: 6a2aee58a41fa007d07aa55565e2231f9b39b4a9}>]
```

You can also write new objects to the database this way:

```ruby
author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}

Rugged::Commit.create(r,
	:author => author,
	:message => "Hello world\n\n",
	:committer => author,
	:parents => ["2cb831a8aea28b2c1b9c63385585b864e4d3bad1"],
	:tree => some_tree,
	:update_ref => "HEAD") #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
```

### Tag Objects

```ruby
tag  = repo.lookup(tag_sha)

object = tag.target
sha    = tag.target.oid
str    = tag.target_type # :commit, :tag, :blob
str    = tag.name        # "v1.0"
str    = tag.message
person = tag.tagger
```

### Tree Objects

```ruby
tree = repo.lookup('779fbb1e17e666832773a9825875300ea736c2da')
# => #<Rugged::Tree:2245194360>

# number of tree entries
tree.count

tree[0]           # or...
tree.first        # or...
tree.get_entry(0)
# => {:type=>:blob, :oid=>"99e7edb53db9355f10c6f2dfaa5a183f205d93bf", :filemode=>33188, :name=>".gitignore"}
```

The tree object is an Enumerable, so you can also do stuff like this:

```ruby
tree.each { |e| puts e[:oid] }
tree.sort { |a, b| a[:oid] <=> b[:oid] }.map { |e| e[:name] }.join(':')
```

And there are some Rugged-specific methods, too:

```ruby
tree.each_tree { |entry| puts entry[:name] }  # list subdirs
tree.each_blob { |entry| puts entry[:name] }  # list only files
```

You can also write trees with the `TreeBuilder`:

```ruby
oid = repo.write("This is a blob.", :blob)
builder = Rugged::Tree::Builder.new(repo)
builder << { :type => :blob, :name => "README.md", :oid => oid, :filemode => 0100644 }

options = {}
options[:tree] = builder.write

options[:author] = { :email => "testuser@github.com", :name => 'Test Author', :time => Time.now }
options[:committer] = { :email => "testuser@github.com", :name => 'Test Author', :time => Time.now }
options[:message] ||= "Making a commit via Rugged!"
options[:parents] = repo.empty? ? [] : [ repo.head.target ].compact
options[:update_ref] = 'HEAD'

Rugged::Commit.create(repo, options)
```

### Blob Objects

Blob objects represent the data in the files of a Tree Object.

```ruby
blob = repo.lookup('e1253910439ea902cf49be8a9f02f3c08d89ac73')
blob.content # => Gives you the content of the blob.
```

#### Streaming Blob Objects

There is currently no way to stream data from a blob, because `libgit2` itself does not (yet) support
streaming blobs out of the git object database. While there are hooks and interfaces for supporting it,
the default file system backend always loads the entire blob contents into memory. 

If you need to access a Blob object through an IO-like API, you can wrap it with the `StringIO` class.
Note that the only advantage here is a stream-compatible interface, the complete blob object will still
be loaded into memory. Below is an example for streaming a Blob using the Sinatra framework:

```ruby
# Sinatra endpoint
get "/blobs/:sha" do
  repo = Rugged::Repository.new(my_repo_path)
  blob = repo.lookup params[:sha]

  headers({
    "Vary" => "Accept",
    "Connection" => "keep-alive",
    "Transfer-Encoding" => "chunked",
    "Content-Type" => "application/octet-stream",
  })

  stream do |out|
    StringIO.new(blob.content).each(8000) do |chunk|
      out << chunk
    end
  end
end
```

---

### Commit Walker

`Rugged::Walker` is a class designed to help you traverse a set of commits over
a repository.

You first push head SHAs onto the walker, and then call next to get a list of
the reachable commit objects one at a time. You can also `hide()` commits if you
are not interested in anything beneath them (useful in situations like when
you're running something like `git log master ^origin/master`).

```ruby
walker = Rugged::Walker.new(repo)
walker.sorting(Rugged::SORT_TOPO | Rugged::SORT_REVERSE) # optional
walker.push(hex_sha_interesting)
walker.hide(hex_sha_uninteresting)
walker.each { |c| puts c.inspect }
walker.reset
```

---

### Index ("staging") area

We can inspect and manipulate the Git Index as well. To work with the index
inside an existing repository, instantiate it by using the `Repository.index`
method instead of manually opening the Index by its path.

```ruby
index = Rugged::Index.new(path)

# Re-read the index file from disk.
index.reload

# Count up index entries.
count = index.count

# The collection of index entries.
index.entries

# Iterating over index entries.
index.each { |i| puts i.inspect }

# Get a particular entry in the index.
index[path]

# Unstage.
index.remove(path)

# Stage. Also updates existing entry if there is one.
index.add(ientry)

# Stage. Create ientry from file in path, updates the index.
index.add(path)
```

---

### Refs

You can access references through the `Rugged::ReferenceCollection` object returned by `Repository#references`.

```ruby
ref = repo.references["refs/heads/master"]

sha = ref.target_id
str = ref.type   # :direct
str = ref.name   # "refs/heads/master"
```

You can also easily iterate over all references:

```ruby
repo.references.each do |ref|
  puts ref.name
end
```

Or only over references that match the given pattern (glob):

```ruby
repo.references.each("refs/tags/*") do |ref|
  puts ref.name
end
```

It is also easy to create, update, rename or delete a reference:

```ruby
ref = repo.references.create("refs/heads/unit_test", some_commit_sha)

repo.references.update(ref, new_sha) # or...
repo.references.update("refs/heads/unit_test", new_sha)

repo.references.rename(ref, "refs/heads/blead") # or...
repo.references.rename("refs/heads/unit_test", "refs/heads/blead")

repo.references.delete(ref) # or...
repo.references.delete("refs/heads/unit_test") # or...
```

Finally, you can access the reflog for any branch:

```ruby
ref = repo.references["refs/heads/master"]
entry = ref.log.first
sha   = entry[:id_old]
sha   = entry[:id_new]
str   = entry[:message]
prsn  = entry[:committer]
```

---

### Branches

The `Rugged::BranchCollection` object returned by `Repository#branches` will help
you with all of your branch-related needs.

Iterate over all branches:

```ruby
repo.branches.each_name().sort
# => ["master", "origin/HEAD", "origin/master", "origin/packed"]

repo.branches.each_name(:local).sort
# => ["master"]

repo.branches.each_name(:remote).sort
# => ["origin/HEAD", "origin/master", "origin/packed"]
```

Look up branches and get attributes:

```ruby
branch = repo.branches["master"]
branch.name # => 'master'
branch.canonical_name # => 'refs/heads/master'
```

Look up the id for the target of a branch:

```ruby
repo.branches["master"].target_id
# => "36060c58702ed4c2a40832c51758d5344201d89a"
```

Creation and deletion:

```ruby
branch = repo.branches.create("test_branch", "HEAD")

repo.branches.rename("test_branch", "new_branch") # or...
repo.branches.rename("refs/heads/test_branch", "new_branch") # or...
repo.branches.rename(ref, "new_branch") # or...

repo.branches.delete("test_branch") # or...
repo.branches.delete("refs/heads/test_branch") # or...
repo.branches.delete(ref) # or...
```

---

### Diffs

There are various ways to get hands on diffs:

```ruby
# Diff between two subsequent commits
diff_commits = commit_object.parents[0].diff(commit_object)

# Diff between two tree objects
diff_trees = tree_object_a.diff(tree_object_b)

# Diff between index/staging and current working directory
diff_index = repository.index.diff

# Diff between index/staging and another diffable (commit/tree/index)
diff_index_diffable = repository.index.diff(some_diffable)
```

When you already have a diff object, you can examine it:

```ruby
# Get patch
diff.patch
=> "diff --git a/foo1 b/foo1\nnew file mode 100644\nindex 0000000..81b68f0\n--- /dev/null\n+++ b/foo1\n@@ -0,0 +1,2 @@\n+abc\n+add line1\ndiff --git a/txt1 b/txt1\ndeleted file mode 100644\nindex 81b68f0..0000000\n--- a/txt1\n+++ /dev/null\n@@ -1,2 +0,0 @@\n-abc\n-add line1\ndiff --git a/txt2 b/txt2\nindex a7bb42f..a357de7 100644\n--- a/txt2\n+++ b/txt2\n@@ -1,2 +1,3 @@\n abc2\n add line2-1\n+add line2-2\n"

# Get delta (faster, if you only need information on what files changed)
diff.each_delta{ |d| puts d.inspect }
#<Rugged::Diff::Delta:70144372137380 {old_file: {:oid=>"0000000000000000000000000000000000000000", :path=>"foo1", :size=>0, :flags=>6, :mode=>0}, new_file: {:oid=>"81b68f040b120c9627518213f7fc317d1ed18e1c", :path=>"foo1", :size=>14, :flags=>6, :mode=>33188}, similarity: 0, status: :added>
#<Rugged::Diff::Delta:70144372136540 {old_file: {:oid=>"81b68f040b120c9627518213f7fc317d1ed18e1c", :path=>"txt1", :size=>14, :flags=>6, :mode=>33188}, new_file: {:oid=>"0000000000000000000000000000000000000000", :path=>"txt1", :size=>0, :flags=>6, :mode=>0}, similarity: 0, status: :deleted>
#<Rugged::Diff::Delta:70144372135780 {old_file: {:oid=>"a7bb42f71183c162efea5e4c80597437d716c62b", :path=>"txt2", :size=>17, :flags=>6, :mode=>33188}, new_file: {:oid=>"a357de7d870823acc3953f1b2471f9c18d0d56ea", :path=>"txt2", :size=>29, :flags=>6, :mode=>33188}, similarity: 0, status: :modified>

# Detect renamed files
# Note that the status field changed from :added/:deleted to :renamed
diff.find_similar!
diff.each_delta{ |d| puts d.inspect }
#<Rugged::Diff::Delta:70144372230920 {old_file: {:oid=>"81b68f040b120c9627518213f7fc317d1ed18e1c", :path=>"txt1", :size=>14, :flags=>6, :mode=>33188}, new_file: {:oid=>"81b68f040b120c9627518213f7fc317d1ed18e1c", :path=>"foo1", :size=>14, :flags=>6, :mode=>33188}, similarity: 100, status: :renamed>
#<Rugged::Diff::Delta:70144372230140 {old_file: {:oid=>"a7bb42f71183c162efea5e4c80597437d716c62b", :path=>"txt2", :size=>17, :flags=>6, :mode=>33188}, new_file: {:oid=>"a357de7d870823acc3953f1b2471f9c18d0d56ea", :path=>"txt2", :size=>29, :flags=>6, :mode=>33188}, similarity: 0, status: :modified>

# Merge one diff into another (mutating the first one)
diff1.merge!(diff2)

# Write a patch into a file (or any other object responding to write)
# Note that the patch as in diff.patch will be written, it won't be applied
file = File.open('/some/file', 'w')
diff.write_patch(file)
file.close
```

---

### Config files

It's also easy to read and manipulate the Git config file data with Rugged.

```ruby
# Read values
repo.config['core.bare']

# Set values
repo.config['user.name'] = true

# Delete values
repo.config.delete('user.name')
```

---

### General methods

Rugged also includes a general library for handling basic Git operations. One of
these is converting a raw sha (20 bytes) into a readable hex sha (40
characters).

```ruby
Rugged.hex_to_raw('bfde59cdd0dfac1d892814f66a95641abd8a1faf')
# => "\277\336Y\315\320\337\254\035\211(\024\366j\225d\032\275\212\037\257"

Rugged.raw_to_hex("\277\336Y\315\320\337\254\035\211(\024\366j\225d\032\275\212\037\257")
=> "bfde59cdd0dfac1d892814f66a95641abd8a1faf"
```

---

### Alternative backends

You can store bare repositories in alternative backends instead of storing on disk. (see
`redbadger/rugged-redis` for an example of how a rugged backend works).

```ruby
a_backend = MyProject::CustomObjectDB(opt1: 'setting', opt2: 'setting')

repo = Rugged::Repository.init_at('repo_name', :bare, backend: a_backend)

# or

repo = Rugged::Repository.bare('repo_name', backend: a_backend)
```
---

## Contributing

Fork libgit2/rugged on GitHub, make it awesomer (preferably in a branch named
for the topic), send a pull request.


## Development

Simply clone and install:

    $ git clone https://github.com/libgit2/rugged.git
    $ cd rugged
    $ bundle install
    $ rake compile
    $ rake test

## Support

We encourage you to use StackOverflow for any questions or concerns regarding Rugged. Please tag your questions with the [rugged](http://stackoverflow.com/questions/tagged/rugged) keyword.

For bug reports, please open a ticket on the GitHub [issue tracker](https://github.com/libgit2/rugged/issues).

## Authors

* Vicent Marti <tanoku@gmail.com>
* Scott Chacon <schacon@gmail.com>
* Arthur Schreiber <schreiber.arthur@gmail.com>


## License

MIT. See LICENSE file.
