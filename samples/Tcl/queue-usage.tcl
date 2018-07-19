set Q [list]
empty Q     ;# ==> 1 (true)
push Q foo
empty Q     ;# ==> 0 (false)
push Q bar
peek Q      ;# ==> foo
pop Q       ;# ==> foo
peek Q      ;# ==> bar
