set date "March 7 2009 7:30pm EST"
set epoch [clock scan $date -format "%B %d %Y %I:%M%p %z"]
set later [clock add $epoch 12 hours]
puts [clock format $later] ;# Sun Mar 08 08:30:00 EDT 2009
puts [clock format $later -timezone :Asia/Shanghai] ;# Sun Mar 08 20:30:00 CST 2009
