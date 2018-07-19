package require struct::prioqueue

set pq [struct::prioqueue]
foreach {priority task} {
    3 "Clear drains"
    4 "Feed cat"
    5 "Make tea"
    1 "Solve RC tasks"
    2 "Tax return"
} {
    # Insert into the priority queue
    $pq put $task $priority
}
# Drain the queue, in priority-sorted order
while {[$pq size]} {
    # Remove the front-most item from the priority queue
    puts [$pq get]
}
