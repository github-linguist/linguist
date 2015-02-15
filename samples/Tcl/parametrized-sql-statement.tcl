package require Tcl 8.6

# These next two lines are the only ones specific to SQLite
package require tdbc::sqlite3
set db [tdbc::sqlite3::connection new /path/to/database.sql]

# Use a helper procedure to make a scope
proc setPlayer {db jersey -> playerName playerScore playerActive} {
    # Note that the '->' above is just syntactic noise for readability
    $db allrows {
	UPDATE players
	SET name = :playerName, score = :playerScore, active = :playerActive
	WHERE jerseyNum = :jersey
    }
    # The named parameters are bound to local variables by default
}

# How to use...
setPlayer $db 99 -> "Smith, Steve" 42 true
# With apologies to http://xkcd.com/327/
setPlayer $db 76 -> "Robert'; DROP TABLE players--" 0 false
$db close
