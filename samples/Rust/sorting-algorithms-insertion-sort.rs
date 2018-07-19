fn insertion_sort<T: std::cmp::Ord>(arr: &mut [T]) {
  for i in range(1, arr.len()) {
    let mut j = i;
    while (j > 0 && arr[j] < arr[j-1]) {
      arr.swap(j, j-1);
      j = j-1;
    }
  }
}
