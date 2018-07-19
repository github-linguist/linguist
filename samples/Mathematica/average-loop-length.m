Grid@Prepend[
  Table[{n, #[[1]], #[[2]],
      Row[{Round[10000 Abs[#[[1]] - #[[2]]]/#[[2]]]/100., "%"}]} &@
    N[{Mean[Array[
        Length@NestWhileList[#, 1, UnsameQ[##] &, All] - 1 &[# /.
            MapIndexed[#2[[1]] -> #1 &,
             RandomInteger[{1, n}, n]] &] &, 10000]],
      Sum[n! n^(n - k - 1)/(n - k)!, {k, n}]/n^(n - 1)}, 5], {n, 1,
    20}], {"N", "average", "analytical", "error"}]
