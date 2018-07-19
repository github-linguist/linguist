void heapSort(List a) {
  int count = a.length;

  // first place 'a' in max-heap order
  heapify(a, count);

  int end = count - 1;
  while (end > 0) {
    // swap the root (maximum value) of the heap with the
    // last element of the heap
    int tmp = a[end];
    a[end] = a[0];
    a[0] = tmp;

    // put the heap back in max-heap order
    siftDown(a, 0, end - 1);

    // decrement the size of the heap so that the previous
    // max value will stay in its proper place
    end--;
  }
}



void heapify(List a, int count) {
  // start is assigned the index in 'a' of the last parent node
  int start = ((count - 2)/2).toInt(); // binary heap

  while (start >= 0) {
    // sift down the node at index 'start' to the proper place
    // such that all nodes below the 'start' index are in heap
    // order
    siftDown(a, start, count - 1);
    start--;
  }
}

void siftDown(List a, int start, int end) {
  // end represents the limit of how far down the heap to shift
  int root = start;

  while ((root*2 + 1) <= end) { // While the root has at least one child
    int child = root*2 + 1; // root*2+1 points to the left child
    // if the child has a sibling and the child's value is less than its sibling's...
    if (child + 1 <= end && a[child] < a[child + 1]) {
      child = child+1; // .. then point to the right child instead
    }

    if (a[root] < a[child]) { // out of max-heap order
      int tmp = a[root];
      a[root] = a[child];
      a[child] = tmp;
      root = child; // repeat to continue shifting down the child now
    } else {
      return;
    }
  }

}

void main() {
  var arr=[1,5,2,7,3,9,4,6,8];
  print("Before sort");
  arr.forEach((var i)=>print("$i"));
  heapSort(arr);
  print("After sort");
  arr.forEach((var i)=>print("$i"));
}
