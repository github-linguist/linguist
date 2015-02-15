import std.stdio, std.typetuple, std.range;

TA[] select(TA, TI1, TC1, TI2, TC2, TI3, TC3, TP)
           (lazy TA mapper,
            ref TI1 iter1, TC1 items1,
            ref TI2 iter2, lazy TC2 items2,
            ref TI3 iter3, lazy TC3 items3,
            lazy TP where) {
  Appender!(TA[]) result;
  auto iters = TypeTuple!(iter1, iter2, iter3);

  foreach (el1; items1) {
    iter1 = el1;
    foreach (el2; items2) {
      iter2 = el2;
      foreach (el3; items3) {
        iter3 = el3;
        if (where())
          result ~= mapper();
      }
    }
  }

  TypeTuple!(iter1, iter2, iter3) = iters;
  return result.data;
}

void main() {
  enum int n = 21;
  int x, y, z;
  auto r = select([x,y,z], x, iota(1,n+1), y, iota(x,n+1), z,
                  iota(y, n + 1), x*x + y*y == z*z);
  writeln(r);
}
