var Josephus = {
  init: function(n) {
    this.head = {};
    var current = this.head;
    for (var i = 0; i < n-1; i++) {
      current.label = i+1;
      current.next = {prev: current};
      current = current.next;
    }
    current.label = n;
    current.next = this.head;
    this.head.prev = current;
    return this;
  },
  kill: function(spacing) {
    var current = this.head;
    while (current.next !== current) {
      for (var i = 0; i < spacing-1; i++) {
        current = current.next;
      }
      current.prev.next = current.next;
      current.next.prev = current.prev;
      current = current.next;
    }
    return current.label;
  }
}
