set1 := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
set2 := [10, 20, 30, 40, 50, 60, 70, 80, 90, 100];

r1 := {INTEGER1 fred};
r2 := {INTEGER1 fred, INTEGER1 sue};
ds1 := DATASET(set1, r1);

ds2 := DATASET(set2, r1);

r2 XF(ds1 L, ds2 R) := TRANSFORM
  SELF.fred := L.fred;
  SELF.sue := R.fred;
END; 

j := JOIN(ds1, ds2, RIGHT.fred % 2 = 0, XF(LEFT, RIGHT), ALL);

OUTPUT(j)
