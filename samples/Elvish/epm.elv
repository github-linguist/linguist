use re
use str
use platform

# Verbosity configuration
var debug-mode = $false

# Configuration for common domains
var -default-domain-config = [
  &"github.com"= [
    &method= git
    &protocol= https
    &levels= 2
  ]
  &"bitbucket.org"= [
    &method= git
    &protocol= https
    &levels= 2
  ]
  &"gitlab.com"= [
    &method= git
    &protocol= https
    &levels= 2
  ]
]

#elvdoc:var managed-dir
#
# The path of the `epm`-managed directory.

var managed-dir = (
  if $platform:is-windows {
    put $E:LocalAppData/elvish/lib
  } elif (not-eq $E:XDG_DATA_HOME '') {
    put $E:XDG_DATA_HOME/elvish/lib
  } else {
    put ~/.local/share/elvish/lib
  }
)

# General utility functions

fn -debug {|text|
  if $debug-mode {
    print (styled '=> ' blue)
    echo $text
  }
}

fn -info {|text|
  print (styled '=> ' green)
  echo $text
}

fn -warn {|text|
  print (styled '=> ' yellow)
  echo $text
}

fn -error {|text|
  print (styled '=> ' red)
  echo $text
}

fn dest {|pkg|
  put $managed-dir/$pkg
}

#elvdoc:fn is-installed
#
# ```elvish
# epm:is-installed $pkg
# ```
#
# Returns a boolean value indicating whether the given package is installed.

fn is-installed {|pkg|
  bool ?(test -e (dest $pkg))
}

fn -package-domain {|pkg|
  str:split &max=2 / $pkg | take 1
}

fn -package-without-domain {|pkg|
  str:split &max=2 / $pkg | drop 1 | str:join ''
}

# Merge two maps
fn -merge {|a b|
  keys $b | each {|k| set a[$k] = $b[$k] }
  put $a
}

# Uppercase first letter of a string
fn -first-upper {|s|
  put (echo $s[0] | tr '[:lower:]' '[:upper:]')$s[(count $s[0]):]
}

# Expand tilde at the beginning of a string to the home dir
fn -tilde-expand {|p|
  re:replace "^~" $E:HOME $p
}

# Known method handlers. Each entry is indexed by method name (the
# value of the "method" key in the domain configs), and must contain
# two keys: install and upgrade, each one must be a closure that
# receives two arguments: package name and the domain config entry
#
# - Method 'git' requires the key 'protocol' in the domain config,
#   which has to be 'http' or 'https'
# - Method 'rsync' requires the key 'location' in the domain config,
#   which has to contain the directory where the domain files are
#   stored. It can be any source location understood by the rsync
#   command.
var -method-handler
set -method-handler = [
  &git= [
    &src= {|pkg dom-cfg|
      put $dom-cfg[protocol]"://"$pkg
    }

    &install= {|pkg dom-cfg|
      var dest = (dest $pkg)
      -info "Installing "$pkg
      mkdir -p $dest
      git clone ($-method-handler[git][src] $pkg $dom-cfg) $dest
    }

    &upgrade= {|pkg dom-cfg|
      var dest = (dest $pkg)
      -info "Updating "$pkg
      try {
        git -C $dest pull
      } catch _ {
          -error "Something failed, please check error above and retry."
      }
    }
  ]

  &rsync= [
    &src= {|pkg dom-cfg|
      put (-tilde-expand $dom-cfg[location])/(-package-without-domain $pkg)/
    }

    &install= {|pkg dom-cfg|
      var dest = (dest $pkg)
      var pkgd = (-package-without-domain $pkg)
      -info "Installing "$pkg
      rsync -av ($-method-handler[rsync][src] $pkg $dom-cfg) $dest
    }

    &upgrade= {|pkg dom-cfg|
      var dest = (dest $pkg)
      var pkgd = (-package-without-domain $pkg)
      if (not (is-installed $pkg)) {
        -error "Package "$pkg" is not installed."
        return
      }
      -info "Updating "$pkg
      rsync -av ($-method-handler[rsync][src] $pkg $dom-cfg) $dest
    }
  ]
]

# Return the filename of the domain config file for the given domain
# (regardless of whether it exists)
fn -domain-config-file {|dom|
  put $managed-dir/$dom/epm-domain.cfg
}

# Return the filename of the metadata file for the given package
# (regardless of whether it exists)
fn -package-metadata-file {|pkg|
  put (dest $pkg)/metadata.json
}

fn -write-domain-config {|dom|
  var cfgfile = (-domain-config-file $dom)
  mkdir -p (dirname $cfgfile)
  if (has-key $-default-domain-config $dom) {
    put $-default-domain-config[$dom] | to-json > $cfgfile
  } else {
    -error "No default config exists for domain "$dom"."
  }
}

# Returns the domain config for a given domain parsed from JSON.
# If the file does not exist but we have a built-in
# definition, then we return the default. Otherwise we return $false,
# so the result can always be checked with 'if'.
fn -domain-config {|dom|
  var cfgfile = (-domain-config-file $dom)
  var cfg = $false
  if ?(test -f $cfgfile) {
    # If the config file exists, read it...
    set cfg = (cat $cfgfile | from-json)
    -debug "Read domain config for "$dom": "(to-string $cfg)
  } else {
    # ...otherwise check if we have a default config for the domain, and save it
    if (has-key $-default-domain-config $dom) {
      set cfg = $-default-domain-config[$dom]
      -debug "No existing config for "$dom", using the default: "(to-string $cfg)
    } else {
      -debug "No existing config for "$dom" and no default available."
    }
  }
  put $cfg
}


# Return the method by which a package is installed
fn -package-method {|pkg|
  var dom = (-package-domain $pkg)
  var cfg = (-domain-config $dom)
  if $cfg {
    put $cfg[method]
  } else {
    put $false
  }
}

# Invoke package operations defined in $-method-handler above
fn -package-op {|pkg what|
  var dom = (-package-domain $pkg)
  var cfg = (-domain-config $dom)
  if $cfg {
    var method = $cfg[method]
    if (has-key $-method-handler $method) {
      if (has-key $-method-handler[$method] $what) {
        $-method-handler[$method][$what] $pkg $cfg
      } else {
        fail "Unknown operation '"$what"' for package "$pkg
      }
    } else {
      fail "Unknown method '"$method"', specified in in config file "(-domain-config-file $dom)
    }
  } else {
    -error "No config for domain '"$dom"'."
  }
}

# Uninstall a single package by removing its directory
fn -uninstall-package {|pkg|
  if (not (is-installed $pkg)) {
    -error "Package "$pkg" is not installed."
    return
  }
  var dest = (dest $pkg)
  -info "Removing package "$pkg
  rm -rf $dest
}

######################################################################
# Main user-facing functions

#elvdoc:fn metadata
#
# ```elvish
# epm:metadata $pkg
# ```
#
# Returns a hash containing the metadata for the given package. Metadata for a
# package includes the following base attributes:
#
# -   `name`: name of the package
# -   `installed`: a boolean indicating whether the package is currently installed
# -   `method`: method by which it was installed (`git` or `rsync`)
# -   `src`: source URL of the package
# -   `dst`: where the package is (or would be) installed. Note that this
#     attribute is returned even if `installed` is `$false`.
#
# Additionally, packages can define arbitrary metadata attributes in a file called
# `metadata.json` in their top directory. The following attributes are
# recommended:
#
# -   `description`: a human-readable description of the package
# -   `maintainers`: an array containing the package maintainers, in
#     `Name <email>` format.
# -   `homepage`: URL of the homepage for the package, if it has one.
# -   `dependencies`: an array listing dependencies of the current package. Any
#     packages listed will be installed automatically by `epm:install` if they are
#     not yet installed.

# Read and parse the package metadata, if it exists
fn metadata {|pkg|
  # Base metadata attributes
  var res = [
    &name= $pkg
    &method= (-package-method $pkg)
    &src= (-package-op $pkg src)
    &dst= (dest $pkg)
    &installed= (is-installed $pkg)
  ]
  # Merge with package-specified attributes, if any
  var file = (-package-metadata-file $pkg)
  if (and (is-installed $pkg) ?(test -f $file)) {
    set res = (-merge (cat $file | from-json) $res)
  }
  put $res
}

#elvdoc:fn query
#
# ```elvish
# epm:query $pkg
# ```
#
# Pretty print the available metadata of the given package.

# Print out information about a package
fn query {|pkg|
  var data = (metadata $pkg)
  var special-keys = [name method installed src dst]
  echo (styled "Package "$data[name] cyan)
  if $data[installed] {
    echo (styled "Installed at "$data[dst] green)
  } else {
    echo (styled "Not installed" red)
  }
  echo (styled "Source:" blue) $data[method] $data[src]
  keys $data | each {|key|
    if (not (has-value $special-keys $key)) {
      var val = $data[$key]
      if (eq (kind-of $val) list) {
        set val = (str:join ", " $val)
      }
      echo (styled (-first-upper $key)":" blue) $val
    }
  }
}

#elvdoc:fn installed
#
# ```elvish
# epm:installed
# ```
#
# Return an array with all installed packages. `epm:list` can be used as an alias
# for `epm:installed`.

# List installed packages
fn installed {
  put $managed-dir/*[nomatch-ok] | each {|dir|
    var dom = (str:replace $managed-dir/ '' $dir)
    var cfg = (-domain-config $dom)
    # Only list domains for which we know the config, so that the user
    # can have his own non-package directories under ~/.elvish/lib
    # without conflicts.
    if $cfg {
      var lvl = $cfg[levels]
      var pat = '^\Q'$managed-dir'/\E('(repeat (+ $lvl 1) '[^/]+' | str:join '/')')/$'
      put (each {|d| re:find $pat $d } [ $managed-dir/$dom/**[nomatch-ok]/ ] )[groups][1][text]
    }
  }
}

# epm:list is an alias for epm:installed
fn list { installed }

#elvdoc:fn install
#
# ```elvish
# epm:install &silent-if-installed=$false $pkg...
# ```
#
# Install the named packages. By default, if a package is already installed, a
# message will be shown. This can be disabled by passing
# `&silent-if-installed=$true`, so that already-installed packages are silently
# ignored.

# Install and upgrade are method-specific, so we call the
# corresponding functions using -package-op
fn install {|&silent-if-installed=$false @pkgs|
  if (eq $pkgs []) {
    -error "You must specify at least one package."
    return
  }
  for pkg $pkgs {
    if (is-installed $pkg) {
      if (not $silent-if-installed) {
        -info "Package "$pkg" is already installed."
      }
    } else {
      -package-op $pkg install
      # Check if there are any dependencies to install
      var metadata = (metadata $pkg)
      if (has-key $metadata dependencies) {
        var deps = $metadata[dependencies]
        -info "Installing dependencies: "(str:join " " $deps)
        # If the installation of dependencies fails, uninstall the
        # target package (leave any already-installed dependencies in
        # place)
        try {
          install $@deps
        } catch e {
          -error "Dependency installation failed. Uninstalling "$pkg", please check the errors above and try again."
          -uninstall-package $pkg
        }
      }
    }
  }
}

#elvdoc:fn upgrade
#
# ```elvish
# epm:upgrade $pkg...
# ```
#
# Upgrade named packages. If no package name is given, upgrade all installed
# packages.

fn upgrade {|@pkgs|
  if (eq $pkgs []) {
    set pkgs = [(installed)]
    -info 'Upgrading all installed packages'
  }
  for pkg $pkgs {
    if (not (is-installed $pkg)) {
      -error "Package "$pkg" is not installed."
    } else {
      -package-op $pkg upgrade
    }
  }
}

#elvdoc:fn uninstall
#
# ```elvish
# epm:uninstall $pkg...
# ```
#
# Uninstall named packages.

# Uninstall is the same for everyone, just remove the directory
fn uninstall {|@pkgs|
  if (eq $pkgs []) {
    -error 'You must specify at least one package.'
    return
  }
  for pkg $pkgs {
    -uninstall-package $pkg
  }
}
