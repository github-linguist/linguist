package require Expect
# or
package require Tclx

for {set i 0} {$i < 100} {incr i} {
    set pid [fork]
    switch $pid {
        -1 {
            puts "Fork attempt #$i failed."
        }
        0 {
            puts "I am child process #$i."
            exit
        }
        default {
            puts "The parent just spawned child process #$i."
        }
    }
}
