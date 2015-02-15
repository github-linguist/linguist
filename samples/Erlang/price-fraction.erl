priceFraction(N) when N < 0 orelse N > 1 ->
    erlang:error('Values must be between 0 and 1.');
priceFraction(N) when N < 0.06 -> 0.10;
priceFraction(N) when N < 0.11 -> 0.18;
priceFraction(N) when N < 0.16 -> 0.26;
priceFraction(N) when N < 0.21 -> 0.32;
priceFraction(N) when N < 0.26 -> 0.38;
priceFraction(N) when N < 0.31 -> 0.44;
priceFraction(N) when N < 0.36 -> 0.50;
priceFraction(N) when N < 0.41 -> 0.54;
priceFraction(N) when N < 0.46 -> 0.58;
priceFraction(N) when N < 0.51 -> 0.62;
priceFraction(N) when N < 0.56 -> 0.66;
priceFraction(N) when N < 0.61 -> 0.70;
priceFraction(N) when N < 0.66 -> 0.74;
priceFraction(N) when N < 0.71 -> 0.78;
priceFraction(N) when N < 0.76 -> 0.82;
priceFraction(N) when N < 0.81 -> 0.86;
priceFraction(N) when N < 0.86 -> 0.90;
priceFraction(N) when N < 0.91 -> 0.94;
priceFraction(N) when N < 0.96 -> 0.98;
priceFraction(N) -> 1.00.
