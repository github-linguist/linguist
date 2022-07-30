
# Usage

<br>

## Application

*Linguist can be used in your application as follows.*

```ruby
require 'rugged'
require 'linguist'

repo = Rugged::Repository.new('.')

project = Linguist::Repository.new(repo,repo.head.target_id)
project.language       #=> "Ruby"
project.languages      #=> { "Ruby" => 119387 }
```

<br>
<br>

## Command Line

### Git Repository

A repository's languages stats can also be assessed from <br>
the command line using the `github-linguist` executable.

Without any options, `github-linguist` will output <br>
the language breakdown by percentage and file size.

```shell
cd /path-to-repository
github-linguist
```

<br>

#### Example

You can try running `github-linguist` on <br>
the root directory of this repository itself:

```console
$ github-linguist
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
```

<br>

### Options

#### `-b`  `--breakdown`

Shows a breakdown of files by language.

<details><summary><b>Example</b></summary>

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

</details>

<br>

#### `-j`  `--json`

Outputs the data in JSON format.

<details><summary><b>Example</b></summary>

```console
$ github-linguist --json
{"Dockerfile":{"size":1212,"percentage":"0.31"},"Ruby":{"size":264519,"percentage":"66.84"},"C":{"size":97685,"percentage":"24.68"},"Lex":{"size":5098,"percentage":"1.29"},"Shell":{"size":1257,"percentage":"0.32"},"Go":{"size":25999,"percentage":"6.57"}}
```

</details>

<br>

*Can be combined with the breakdown option.*

<details><summary><b>Example</b></summary>

```console
$ github-linguist --breakdown --json
{"Dockerfile":{"size":1212,"percentage":"0.31","files":["Dockerfile","tools/grammars/Dockerfile"]},"Ruby":{"size":264519,"percentage":"66.84","files":["Gemfile","Rakefile","bin/git-linguist","bin/github-linguist","ext/linguist/extconf.rb","github-linguist.gemspec","lib/linguist.rb",...]}}

```

</details>

<br>

### Single File

To only process a single file, simply <br>
call Linguist with the path / filename.

<details><summary><b>Example</b></summary>

```console
$ github-linguist grammars.yml
grammars.yml: 884 lines (884 sloc)
  type:      Text
  mime type: text/x-yaml
  language:  YAML
```

</details>

<br>

### Docker

If you have Docker installed you can build an <br>
image and run Linguist within a container:

```console
$ docker build -t linguist .
$ docker run --rm -v $(pwd):$(pwd) -w $(pwd) -t linguist
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
```

```console
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

<br>
