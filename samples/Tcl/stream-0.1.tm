# A stream ensemble
#
# Copyright (c) 2013 Lawrence Woodman <lwoodman@vlifesystems.com>
#
# Licensed under an MIT licence.  Please see LICENCE.md for details.
#

package require Tcl 8.5

namespace eval stream {
  namespace export {[a-z]*}
  namespace ensemble create
}

proc stream::create {first restCmdPrefix} {
  return [list $first $restCmdPrefix]
}

proc stream::first {stream} {
  lassign $stream first
  return $first
}

proc stream::foldl {cmdPrefix initialValue args} {
  set numStreams [llength $args]
  if {$numStreams == 1} {
    FoldlSingleStream $cmdPrefix $initialValue [lindex $args 0]
  } elseif {$numStreams > 1} {
    FoldlMultiStream $cmdPrefix $initialValue $args
  } else {
    Usage "stream foldl cmdPrefix initalValue stream ?stream ..?"
  }
}

proc stream::foreach {args} {
  set numArgs [llength $args]
  if {$numArgs == 3} {
    lassign $args varName stream body
    ForeachSingleStream $varName $stream $body
  } elseif {($numArgs > 3) && (($numArgs % 2) == 1)} {
    set body [lindex $args end]
    set items [lrange $args 0 end-1]
    ForeachMultiStream $items $body
  } else {
    Usage "stream foreach varName stream ?varName stream ..? body"
  }
}

proc stream::fromList {_list {index 0}} {
  if {$index >= [llength $_list]} {return {}}
  create [lindex $_list $index] [list fromList $_list [expr {$index + 1}]]
}

proc stream::isEmpty {stream} {
  expr {[llength $stream] == 0}
}

proc stream::map {cmdPrefix args} {
  set numArgs [llength $args]
  if {$numArgs == 1} {
    MapSingleStream $cmdPrefix [lindex $args 0]
  } elseif {$numArgs > 1} {
    MapMultiStream $cmdPrefix $args
  } else {
    Usage "stream map cmdPrefix stream ?stream ..?"
  }
}

proc stream::rest {stream} {
  set rest [lindex $stream 1]
  {*}$rest
}

# Note: This will work through the elements of the stream until it finds
# the first element that is matched by the cmdPrefix predicate.
proc stream::select {cmdPrefix stream} {
  while {![isEmpty $stream]} {
    lassign $stream first rest
    if {[{*}$cmdPrefix $first]} {
      return [create $first [list select $cmdPrefix [{*}$rest]]]
    } else {
      set stream [{*}$rest]
    }
  }
  return $stream
}

proc stream::take {num stream} {
  if {[isEmpty $stream] || $num <= 0} {
    return [::list]
  } else {
    lassign $stream first rest
    create $first [list take [expr {$num - 1}] [{*}$rest]]
  }
}

proc stream::toList {stream} {
  set res [::list]
  while {![isEmpty $stream]} {
    lassign $stream first rest
    lappend res $first
    set stream [{*}$rest]
  }
  return $res
}


#################################
#           Internal
#################################

proc stream::FoldlSingleStream {cmdPrefix initialValue stream} {
  set acc $initialValue
  while {![isEmpty $stream]} {
    lassign $stream first rest
    set acc [{*}$cmdPrefix $acc $first]
    set stream [{*}$rest]
  }
  return $acc
}

proc stream::FoldlMultiStream {cmdPrefix initialValue streams} {
  set acc $initialValue

  while 1 {
    set firsts [::list]
    set restStreams [::list]
    ::foreach stream $streams {
      if {[isEmpty $stream]} {
        return $acc
      }
      lassign $stream first rest
      lappend firsts $first
      lappend restStreams [{*}$rest]
    }
    set acc [{*}$cmdPrefix $acc {*}$firsts]
    set streams $restStreams
  }
  return $acc
}

proc stream::ForeachSingleStream {varName stream body} {
  set res {}
  while {![isEmpty $stream]} {
    lassign $stream first rest
    uplevel 2 [list set $varName $first]
    set stream [{*}$rest]
    set res [uplevel 2 $body]
  }
  return $res
}

proc stream::ForeachMultiStream {items body} {
  set res {}

  while 1 {
    set nextItems [::list]
    ::foreach {varName stream} $items {
      if {[isEmpty $stream]} {
        return $res
      }
      lassign $stream first rest
      uplevel 2 [list set $varName $first]
      lappend nextItems $varName
      lappend nextItems [{*}$rest]
    }
    set res [uplevel 2 $body]
    set items $nextItems
  }

  return $res
}

proc stream::MapSingleStream {cmdPrefix stream} {
  if {[isEmpty $stream]} {
    return $stream
  }
  lassign $stream first rest
  create [{*}$cmdPrefix $first] [list MapSingleStream $cmdPrefix [{*}$rest]]
}

proc stream::MapMultiStream {cmdPrefix streams} {
  set firsts [::list]
  set restStreams [::list]

  ::foreach stream $streams {
    if {[isEmpty $stream]} {
      return $stream
    }
    lassign $stream first rest
    lappend firsts $first
    lappend restStreams [{*}$rest]
  }

  return [create [{*}$cmdPrefix {*}$firsts] \
                 [list MapMultiStream $cmdPrefix $restStreams]]
}

proc stream::Usage {msg} {
  return -code error -level 2 "wrong # args: should be \"$msg\""
}
