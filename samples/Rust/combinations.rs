fn comb<T: std::fmt::Default>(arr: &[T], n: uint) {
  let mut incl_arr: ~[bool] = std::vec::from_elem(arr.len(), false);
  comb_intern(arr, n, incl_arr, 0);
}

fn comb_intern<T: std::fmt::Default>(arr: &[T], n: uint, incl_arr: &mut [bool], index: uint) {
  if (arr.len() < n + index) { return; }
  if (n == 0) {
    let mut it = arr.iter().zip(incl_arr.iter()).filter_map(|(val, incl)|
      if (*incl) { Some(val) } else { None }
    );
    for val in it { print!("{} ", *val); }
    print("\n");
    return;
  }

  incl_arr[index] = true;
  comb_intern(arr, n-1, incl_arr, index+1);
  incl_arr[index] = false;

  comb_intern(arr, n, incl_arr, index+1);
}

fn main() {
  let arr1 = ~[1, 2, 3, 4, 5];
  comb(arr1, 3);

  let arr2 = ~["A", "B", "C", "D", "E"];
  comb(arr2, 3);
}
