public void move(int n, int from, int to, int via) {
  if (n == 1) {
    System.out.println("Move disk from pole " + from + " to pole " + to);
  } else {
    move(n - 1, from, via, to);
    move(1, from, to, via);
    move(n - 1, via, to, from);
  }
}
