import groovy.transform.Canonical

@Canonical
class Task implements Comparable<Task> {
    int priority
    String name
    int compareTo(Task o) { priority <=> o?.priority }
}

new PriorityQueue<Task>().with {
    add new Task(priority: 3, name: 'Clear drains')
    add new Task(priority: 4, name: 'Feed cat')
    add new Task(priority: 5, name: 'Make tea')
    add new Task(priority: 1, name: 'Solve RC tasks')
    add new Task(priority: 2, name: 'Tax return')

    while (!empty) { println remove() }
}
