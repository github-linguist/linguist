package require Tcl 8.5
package require http
package require json

fconfigure stdout -buffering none

proc get_tasks {category} {
    set start [clock milliseconds]
    puts -nonewline "getting $category members..."
    set base_url http://www.rosettacode.org/w/api.php
    set query {action query list categorymembers cmtitle Category:%s format json cmlimit 500}
    set this_query [dict create {*}[split [format $query $category]]]
    set tasks [list]

    while {1} {
        set url [join [list $base_url [http::formatQuery {*}$this_query]] ?]
        set response [http::geturl $url]
        if {[set s [http::status $response]] ne "ok" || [http::ncode $response] != 200} {
            error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $response]"
        }
        set data [json::json2dict [http::data $response]]
        http::cleanup $response

        # add tasks to list
        foreach task [dict get $data query categorymembers] {
            lappend tasks [dict get [dict create {*}$task] title]
        }

        if {[catch {dict get $data query-continue categorymembers cmcontinue} continue_task] != 0} {
            # no more continuations, we're done
            break
        }
        dict set this_query cmcontinue $continue_task
    }
    puts " found [llength $tasks] tasks in [expr {[clock milliseconds] - $start}] milliseconds"
    return $tasks
}

# This proc can be replaced by a single regexp command:
#     set count [regexp -all "***=$needle" $haystack]
# However this proc is more efficient -- we're dealing with plain strings only.
proc count_substrings {needle haystack} {
    set count 0
    set idx 0
    while {[set idx [string first $needle $haystack $idx]] != -1} {
        incr count
        incr idx
    }
    return $count
}

set total 0
foreach task [get_tasks Programming_Tasks] {
    set url [format "http://www.rosettacode.org/w/index.php?title=%s&action=raw" [string map {{ } _} $task]]
    set response [http::geturl $url]
    if {[set s [http::status $response]] ne "ok" || [http::ncode $response] != 200} {
        error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $response]"
    }
    set count [count_substrings "\{\{header|" [http::data $response]]
    puts [format "%3d examples in %s" $count $task]
    http::cleanup $response
    incr total $count
}

puts "\nTotal: $total examples"
