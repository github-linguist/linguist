# Warning!
This source is intended to be used when all of a projects dependencies have been vendored and does not detect non-vendored packages installed at `$GOPATH/pkg`.  If your project uses dependencies that are not listed in `Gopkg.lock`, then you must use the go source to enumerate all project dependencies.

# Go Dep

The dep source will detect dependencies when the source is enabled and `Gopkg.lock` is found at an apps `source_path`.  It
parses the `Gopkg.lock` file to find packages that have been vendored into the project directory.

#### Limitations

The dep dependency source has some limitations compared to the general-purpose go source.
1. Go std libraries are not filtered from enumerated dependencies if `go list std` is not available
2. Summary information is not available for packages

#### Go or Dep

Reasons to choose the dep source over the go source
1. The dep source does not have a hard dependency on go being installed
  - some functionality is only available if go is available
    1. filtering go std libs from the found dependencies
2. The dep source should generally run much more quickly then the go source

Reasons to choose the go source over the dep source
1. Your project has dependencies not specified by `Gopkpg.lock`
2. You require dependency summary information
