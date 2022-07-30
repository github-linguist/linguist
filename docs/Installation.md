
# Installation

<br>

## Ruby

You will need a recent version of **[Ruby]**.

<br>

### MacOS / XCode

There are known problems with the supplied versions of <br>
Ruby from these environments that can causes problems <br>
with installing some of the dependencies.

<br>

### Recommended

Install Ruby using a package managing system such as:

- **[Homebrew]**
- **ruby-build**
- **rbenv**
- **asdf**
- **rvm**

<br>
<br>

## Dependencies

*The following libraries + dependencies are required.*

<br>

### [Charlock Holmes]

Used for character encoding.

-   **pkg-config**
-   **cmake**
-   **[ICU]**
-   **[zlib]**

<br>

### [Rugged]

Ruby bindings for libgit2

-   **[OpenSSL]**
-   **[libcurl]**


<br>
<br>

## Installing

<br>

#### MacOS + [Homebrew]

*Installing dependencies on MacOS.*

```shell
brew install    \
    pkg-config  \
    cmake       \
    icu4c
```

<br>

#### Ubuntu

*Installing dependencies on Ubuntu.*

```shell
sudo apt install            \
    libcurl4-openssl-dev    \
    libssl-dev ruby-dev     \
    build-essential         \
    pkg-config              \
    libicu-dev              \
    zlib1g-dev              \
    cmake
```

<br>

### Linguist

*Installing the Ruby gem.*

```shell
gem install github-linguist
```

<br>


<!----------------------------------------------------------------------------->

[Charlock Holmes]: https://github.com/brianmario/charlock_holmes
[Homebrew]: http://brew.sh/
[libcurl]: https://curl.haxx.se/libcurl/
[OpenSSL]: https://www.openssl.org
[Rugged]: https://github.com/libgit2/rugged
[Ruby]: https://www.ruby-lang.org/en/
[zlib]: https://zlib.net/
[ICU]: http://site.icu-project.org/
