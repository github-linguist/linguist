CountCoins[amount_, coinlist_] := ( ways = ConstantArray[1, amount];
Do[For[j = coin, j <= amount, j++,
  If[ j - coin == 0,
    ways[[j]] ++,
    ways[[j]] += ways[[j - coin]]
]]
, {coin, coinlist}];
ways[[amount]])
