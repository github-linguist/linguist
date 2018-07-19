package require Tcl 8.5
package require http
package require json
package require struct::set

fconfigure stdout -buffering none

# Initialize a cache of lookups
array set cache {}
proc log msg {
    #puts -nonewline $msg
}

proc get_tasks {category} {
    global cache
    if {[info exists cache($category)]} {
	return $cache($category)
    }
    set base_url http://www.rosettacode.org/w/api.php
    set query {
	action	query
	list	categorymembers
	cmtitle	Category:%s
	format	json
	cmlimit	500
    }
    set query [list {*}$query]; # remove excess whitespace
    set this_query [dict create {*}[split [format $query $category]]]
    set tasks [list]

    while {1} {
        set url [join [list $base_url [http::formatQuery {*}$this_query]] ?]
        while 1 {
            set response [http::geturl $url]
	    # Process redirects
            if {[http::ncode $response] == 301} {
                set newurl [dict get [http::meta $response] Location]
                if {[string match http://* $newurl]} {
                    set url $newurl
                } else {
                    set url [regexp -inline {http://[^/]+} $url]
                    append url $newurl
                }
                continue
            }
	    # Check for oopsies!
            if {
		[set s [http::status $response]] ne "ok"
		|| [http::ncode $response] != 200
	    } then {
                error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $response]"
            }
            break
        }

	# Get the data out of the message
        set data [json::json2dict [http::data $response]]
        http::cleanup $response

        # add tasks to list
        foreach task [dict get $data query categorymembers] {
            lappend tasks [dict get [dict create {*}$task] title]
        }

        if {[catch {
	    dict get $data query-continue categorymembers cmcontinue
	} continue_task]} then {
            # no more continuations, we're done
            break
        }
        dict set this_query cmcontinue $continue_task
    }
    return [set cache($category) $tasks]
}

proc get_unimplemented {lang} {
    set tasks [get_tasks Programming_Tasks]
    set collected [get_tasks Collection_Members]
    set doneTasks [get_tasks $lang]
    set omittedTasks [get_tasks $lang/Omit]

    # Map generic collection task categories to specific ones
    set tasks [regsub -all {Category:(\S+)} $tasks "\\1/$lang"]

    set collectOfLang [struct::set intersect $collected $doneTasks]
    set ignorable [struct::set union $doneTasks $omittedTasks $collectOfLang]
    set unimplemented [struct::set difference $tasks $ignorable]

    puts "\n$lang has [llength $unimplemented] unimplemented programming tasks:"
    if {[llength $unimplemented]} {
	puts "  [join [lsort $unimplemented] "\n  "]"
    }
}

foreach lang {Perl Python Ruby Tcl} {
    get_unimplemented $lang
}
