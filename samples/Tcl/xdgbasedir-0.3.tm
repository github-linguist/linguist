# XDG Base Directory Specification handling
#
# Copyright (C) 2013 Lawrence Woodman
#
# Licensed under an MIT licence.  Please see LICENCE.md for details.
#
# For XDG Base Directory Specification
#   http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
#
package require Tcl 8.5

namespace eval XDG {
  variable DEFAULTS ""
  namespace export DATA_HOME CONFIG_HOME CACHE_HOME
  namespace export RUNTIME_DIR DATA_DIRS CONFIG_DIRS
}

proc XDG::SetDefaults {} {
  variable DEFAULTS
  if {$DEFAULTS ne ""} return
  set DEFAULTS [list \
    DATA_HOME   [file join $::env(HOME) .local share] \
    CONFIG_HOME [file join $::env(HOME) .config] \
    CACHE_HOME  [file join $::env(HOME) .cache] \
    DATA_DIRS   [list [file join /usr local share] [file join /usr share]] \
    CONFIG_DIRS [list [file join /etc xdg ]]
  ]
}

proc XDG::XDGVarSet {var} {
  expr {[info exists ::env(XDG_$var)] && $::env(XDG_$var) ne ""}
}

proc XDG::Dir {var {subdir ""} } {
  variable DEFAULTS
  SetDefaults
  set dir [dict get $DEFAULTS $var]

  if {[XDGVarSet $var]} {
    set dir $::env(XDG_$var)
  }

  return [file join $dir $subdir]
}

proc XDG::Dirs {var {subdir ""} } {
  variable DEFAULTS
  SetDefaults
  set rawDirs [dict get $DEFAULTS $var]

  if {[XDGVarSet $var]} {
    set rawDirs [split $::env(XDG_$var) ":"]
  }

  set outDirs {}
  foreach dir $rawDirs {
    lappend outDirs [file join $dir $subdir]
  }
  return $outDirs
}

# The remaining procs reference the environmental variables XDG_
# followed by the proc name.
proc XDG::DATA_HOME {{subdir ""}} {Dir DATA_HOME $subdir}
proc XDG::CONFIG_HOME {{subdir ""}} {Dir CONFIG_HOME $subdir}
proc XDG::CACHE_HOME {{subdir ""}} {Dir CACHE_HOME $subdir}

proc XDG::RUNTIME_DIR {{subdir ""}} {
  if {![XDGVarSet RUNTIME_DIR]} { return {} }
  return [file join $::env(XDG_RUNTIME_DIR) $subdir]
}

# The following procs returning the directories as a list with the most
# important first.
proc XDG::DATA_DIRS {{subdir ""}} {Dirs DATA_DIRS $subdir}
proc XDG::CONFIG_DIRS {{subdir ""}} {Dirs CONFIG_DIRS $subdir}
