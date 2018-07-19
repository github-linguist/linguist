merge(left, right, items) {
  var a = 0;
  var t;

  while (left.length != 0 && right.length != 0) {
    if (right[0] < left[0]) {
     t = right[0];
     right.removeRange(0,1);
    } else {
      t = left[0];
      left.removeRange(0,1);
    }
    items[a++] = t;
  }

  while(left.length != 0) {
    t = left[0];
    left.removeRange(0,1);
    items[a++] = t;
  }

  while(right.length != 0) {
    t = right[0];
    right.removeRange(0,1);
    items[a++] = t;
  }
}

mSort(items, tmp, l) {
  if (l == 1) {
    return;
  }

  var m = (l/2).floor().toInt();
  var tmp_l = tmp.getRange(0, m);
  var tmp_r = tmp.getRange(m, tmp.length-m);

  mSort(tmp_l, items.getRange(0,m), m);
  mSort(tmp_r, items.getRange(m, items.length-m), l-m);
  merge(tmp_l, tmp_r, items);
}

merge_sort(items) {
  mSort(items,items.getRange(0, items.length),items.length);
}

void main() {
  var arr=[1,5,2,7,3,9,4,6,8];
  print("Before sort");
  arr.forEach((var i)=>print("$i"));
  merge_sort(arr);
  print("After sort");
  arr.forEach((var i)=>print("$i"));
}
