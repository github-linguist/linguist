## Installation

Install the gem:

```bash
gem install github-linguist
```

### Dependencies

Linguist is a Ruby library so you will need a recent version of Ruby installed.
There are known problems with the macOS/XCode supplied version of Ruby that causes problems installing some of the dependencies.
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
