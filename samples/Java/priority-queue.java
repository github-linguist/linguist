import java.util.PriorityQueue;

class Task implements Comparable<Task> {
    final int priority;
    final String name;

    public Task(int p, String n) {
        priority = p;
        name = n;
    }

    public String toString() {
        return priority + ", " + name;
    }

    public int compareTo(Task other) {
        return priority < other.priority ? -1 : priority > other.priority ? 1 : 0;
    }

    public static final void main(String[] args) {
        PriorityQueue<Task> pq = new PriorityQueue<Task>();
        pq.add(new Task(3, "Clear drains"));
        pq.add(new Task(4, "Feed cat"));
        pq.add(new Task(5, "Make tea"));
        pq.add(new Task(1, "Solve RC tasks"));
        pq.add(new Task(2, "Tax return"));

        while (!pq.isEmpty())
            System.out.println(pq.remove());
    }
}
