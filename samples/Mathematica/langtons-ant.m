direction = 1;
data = SparseArray[{{50, 50} -> -1}, {100, 100}, 1];
NestWhile[
  {Re@#, Im@#} &@(direction *= (data[[Sequence @@ #]] *= -1) I) + # &,
  {50, 50}, 1 <= Min@# <= Max@# <= 100 &];
Image@data
