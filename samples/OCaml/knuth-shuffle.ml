let shuffle arr =
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done
