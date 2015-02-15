/**
Return true if queen placement q[n] does not conflict with
other queens q[0] through q[n-1]
*/
isConsistent(List q, int n) {
  for (int i=0; i<n; i++) {
    if (q[i] == q[n]) {
      return false; // Same column
    }

    if ((q[i] - q[n]) == (n - i)) {
      return false; // Same major diagonal
    }

    if ((q[n] - q[i]) == (n - i)) {
      return false; // Same minor diagonal
    }
  }

  return true;
}

/**
Print out N-by-N placement of queens from permutation q in ASCII.
*/
printQueens(List q) {
  int N = q.length;
  for (int i=0; i<N; i++) {
    StringBuffer sb = new StringBuffer();
    for (int j=0; j<N; j++) {
      if (q[i] == j) {
        sb.add("Q ");
      } else {
        sb.add("* ");
      }
    }
    print(sb.toString());
  }
  print("");
}

/**
Try all permutations using backtracking
*/
enumerate(int N) {
  var a = new List(N);
  _enumerate(a, 0);
}

_enumerate(List q, int n) {
  if (n == q.length) {
    printQueens(q);
  } else {
    for (int i = 0; i < q.length; i++) {
      q[n] = i;
      if (isConsistent(q, n)){
        _enumerate(q, n+1);
      }
    }
  }
}

void main() {
  enumerate(4);
}
