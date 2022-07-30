## Usage

### Application usage

Linguist can be used in your application as follows:

```ruby
require 'rugged'
require 'linguist'

repo = Rugged::Repository.new('.')
project = Linguist::Repository.new(repo, repo.head.target_id)
project.language       #=> "Ruby"
project.languages      #=> { "Ruby" => 119387 }
```

### Command line usage

#### Git Repository

A repository's languages stats can also be assessed from the command line using the `github-linguist` executable.
Without any options, `github-linguist` will output the language breakdown by percentage and file size.

```bash
cd /path-to-repository
github-linguist
```

You can try running `github-linguist` on the root directory in this repository itself:

```console
$ github-linguist
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
```

#### Additional options

##### `--breakdown`
The `--breakdown` or `-b` flag will additionally show the breakdown of files by language.

You can try running `github-linguist` on the root directory in this repository itself:

```console
$ github-linguist --breakdown
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile

Ruby:
Gemfile
Rakefile
bin/git-linguist
bin/github-linguist
ext/linguist/extconf.rb
github-linguist.gemspec
lib/linguist.rb
…
```

##### `--json`
The `--json` or `-j` flag output the data into JSON format.

```console
$ github-linguist --json
{"Dockerfile":{"size":1212,"percentage":"0.31"},"Ruby":{"size":264519,"percentage":"66.84"},"C":{"size":97685,"percentage":"24.68"},"Lex":{"size":5098,"percentage":"1.29"},"Shell":{"size":1257,"percentage":"0.32"},"Go":{"size":25999,"percentage":"6.57"}}
```

This option can be used in conjunction with `--breakdown` to get a full list of files along with the size and percentage data.
```console
$ github-linguist --breakdown --json
{"Dockerfile":{"size":1212,"percentage":"0.31","files":["Dockerfile","tools/grammars/Dockerfile"]},"Ruby":{"size":264519,"percentage":"66.84","files":["Gemfile","Rakefile","bin/git-linguist","bin/github-linguist","ext/linguist/extconf.rb","github-linguist.gemspec","lib/linguist.rb",...]}}

```

#### Single file

Alternatively you can find stats for a single file using the `github-linguist` executable.

You can try running `github-linguist` on files in this repository itself:

```console
$ github-linguist grammars.yml
grammars.yml: 884 lines (884 sloc)
  type:      Text
  mime type: text/x-yaml
  language:  YAML
```

#### Docker

If you have Docker installed you can build an image and run Linguist within a container:

```console
$ docker build -t linguist .
$ docker run --rm -v $(pwd):$(pwd) -w $(pwd) -t linguist
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
$ docker run --rm -v $(pwd):$(pwd) -w $(pwd) -t linguist github-linguist --breakdown
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile

Ruby:
Gemfile
Rakefile
bin/git-linguist
bin/github-linguist
ext/linguist/extconf.rb
github-linguist.gemspec
lib/linguist.rb
…
```
