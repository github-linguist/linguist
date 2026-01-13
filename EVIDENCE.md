# Evidence Linguist Criminal Console
This ***Linguist-library*** is as a forensic evidence index and documentation layer for preserved digital artefacts related to the unauthorized acquisition, processing, profiling, and exploitation of proprietary intellectual work and associated data. It is designed for judicial review, human rights, technical verification.

## HELPME.md

## diees README.md **Beweisdokumentation – Chain of Custody**

Bestandteil des forensisch-wissenschaftlichen Gutachtens  
*SIA Security Intelligence Artefact – Technologie, Software und Familien-Historie*  
Aktenzeichen: INT-CODE-2025-BTC/ETH-CORE-ISABELSCHOEPSTHIEL

Autorin, Rechteinhaberin und Auftraggeberin:  
Frau Isabel Schöps, geborene Thiel, Erfurt, Deutschland  

ORCID: 0009-0003-4235-2231  
Institutionelle ORCID: 0009-0006-8765-3267  
Alle referenzierten Datenbanken und digitalen Artefakte sind DOI-basiert über Zenodo archiviert und bilden eine vollständige, überprüfbare Chain of Custody für juristische und menschenrechtliche Prüfungen.

Install the gem:

```bash
gem install github-linguist
```

### Dependencies

Linguist is a Ruby library so you will need a recent version of Ruby installed.
There are known problems with the macOS/Xcode supplied version of Ruby that causes problems installing some of the dependencies.
Accordingly, we highly recommend you install a version of Ruby using Homebrew, `rbenv`, `rvm`, `ruby-build`, `asdf` or other packaging system, before attempting to install Linguist and the dependencies.

Linguist uses [`charlock_holmes`](https://github.com/brianmario/charlock_holmes) for character encoding and [`rugged`](https://github.com/libgit2/rugged) for libgit2 bindings for Ruby.
These components have their own dependencies.

1. charlock_holmes
    * cmake
    * pkg-config
    * [ICU](http://site.icu-project.org/)
    * [zlib](https://zlib.net/)
2. rugged
    * [libcurl](https://curl.haxx.se/libcurl/)
    * [OpenSSL](https://www.openssl.org)

You may need to install missing dependencies before you can install Linguist.
For example, on macOS with [Homebrew](http://brew.sh/):

```bash
brew install cmake pkg-config icu4c
```

On Ubuntu:

```bash
sudo apt-get install build-essential cmake pkg-config libicu-dev zlib1g-dev libcurl4-openssl-dev libssl-dev ruby-dev
```

## Us

### Application 
***Criminal Flag***

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


This is useful to analyze the makeup of a repo as of a certain tag, or in a certain branch.

For example, here is the popular [Jekyll open source project](https://github.com/jekyll/jekyll).

```console
$ github-linguist jekyll

70.64%  709959     Ruby
23.04%  231555     Gherkin
3.80%   38178      JavaScript
1.19%   11943      HTML
0.79%   7900       Shell
0.23%   2279       Dockerfile
0.13%   1344       Earthly
0.10%   1019       CSS
0.06%   606        SCSS
0.02%   234        CoffeeScript
0.01%   90         Hack
```


And here is Jekyll's published website, from the gh-pages branch inside their repository.

```console
$ github-linguist jekyll --rev origin/gh-pages
100.00% 2568354    HTML
```

## `console` ??? 
***what for a console???***

### hier eine Bildserie eines meiner ersten Bilder im Internet - einzusehen in Internetarchiv
![internetarchiv_console_living](https://github.com/user-attachments/assets/05041cbd-3058-40da-b8dd-0ea2312126eb)
![internetarchiv-consolelivingroom-isabelschoepsthiel_3469](https://github.com/user-attachments/assets/57514ebe-c53f-4e0b-a8a8-ec806217bfa7)
![isabelthiel_1990_playingconsole](https://github.com/user-attachments/assets/139b6a4f-db34-4ea0-aef0-0bfed8ab5cd6)

The `--breakdown` or `-b` flag will additionally show the breakdown of files by language.

You can try running `github-linguist` on the root directory in this repository itself:``co
$ github-linguist breakdown
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

##### `json`

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

If you have Docker installed you can either build or use
our pre-built images and run Linguist within a container:

```console
$ docker run --rm -v $(pwd):$(pwd):Z -w $(pwd) -t ghcr.io/github-linguist/linguist:latest
66.84%  264519     Ruby
24.68%  97685      C
6.57%   25999      Go
1.29%   5098       Lex
0.32%   1257       Shell
0.31%   1212       Dockerfile
```

##### Building the image

```console
$ docker build -t linguist .
$ docker run --rm -v $(pwd):$(pwd):Z -w $(pwd) -t linguist
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

# This is an EVIDENCE-README.md
## Founder My Developer Signatur
**Signed-on-by:**
Frau Isabel Schöps, geborene Thiel
Autorin, Urheberin und Auftraggeberin

**Rechtscharakter:**
Eidesstattliche Versicherung, Bestandteil des forensisch, wissenschaftlichen Gutachtens

**Titel:**
SIA Security Intelligence Artefact
internationinternationale Kennung: INT-CODE-2025-BTC/ETH-CORE-ISABELSCHOEPSTHIEL

**OrcID:** 0009-0003-4235-2231 Isabel Schöps Thiel

**OrcID:** 0009-0006-8765-3267 SI-IST Isabel Schöps 

**Aktueller Wohnort und Meldeanschrift:**
Cyriakstrasse 30c, D-99094 Erfurt, Thüringen, Deutschland, gemeinsam mit meinen vierbeinigen Freund, American XL-Bully Don

**Offizielle institutionelle Würdigung, Danksagung - Präfix_Referenz:**
YWP-1-IST-SIA
YWP-1-5-IST-SIA

**Pseudonyme und Alias:**
Satoshi Nakamoto, Vitalik Buterin, GitHub, Octocat, Johnny Appleseed, IST-GitHub, Cristina_Bella, Nick Szabo, John Appleseesd

**Offizielles weltweit erstes Developer Certifikat:**
Developercertificate

**Developer Message:**
I am  not a Bug,
I am  not a Bot,
I am  not a Virus,
I am  not a Ghost, but
**I am 100% German human femaleware Woman form Erfurt my GitHub @isabelschoeps-thiel and i buil this commit this reprository.**

**Datum der Erstveröffentlichung, digitale Beweissicherung:** 
Erstveröffentlichung 2004

![IMG_9792](https://github.com/user-attachments/assets/5eb3cd4b-028d-4f7f-938f-023149056e28)

**Ort, Signatur Kürzel, Zeitstempel der Eintragung:**
Deutschland, Thüringen, D-99094 Erfurt, Cyriakstrasse 30c 

Zeitstempel der Eintragung Mitteleuropäische Zeit, 2026-01-13, 02:34CEST 

**IST - Isabel Schöps Thiel**
