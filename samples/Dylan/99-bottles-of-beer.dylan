Module: bottles
define method bottles (n :: <integer>)
  for (n from 99 to 1 by -1)
    format-out("%d bottles of beer on the wall,\n"
               "%d bottles of beer\n"
               "Take one down, pass it around\n"
               "%d bottles of beer on the wall\n",
               n, n, n - 1);
  end
end method
