program FactorsMersenneNumber(input, output);

function isPrime(n: longint): boolean;
  var
    d: longint;
  begin
    isPrime := true;
    if (n mod 2) = 0 then
    begin
      isPrime := (n = 2);
      exit;
    end;
    if (n mod 3) = 0 then
    begin
      isPrime := (n = 3);
      exit;
    end;
    d := 5;
    while d*d <= n do
    begin
      if (n mod d) = 0 then
      begin
	isPrime := false;
	exit;
      end;
      d := d + 2;
    end;
  end;

function btest(n, pos: longint): boolean;
  begin
    btest := (n shr pos) mod 2 = 1;
  end;

function MFactor(p: longint): longint;
  var
    i, k,  maxk, msb, n, q: longint;
  begin
    for i := 30 downto 0 do
      if btest(p, i) then
      begin
	msb := i;
	break;
      end;
    maxk := 16384 div p;     // limit for k to prevent overflow of 32 bit signed integer
    for k := 1 to maxk do
    begin
      q := 2*p*k + 1;
      if not isprime(q) then
	continue;
      if ((q mod 8) <> 1) and ((q mod 8) <> 7) then
	continue;
      n := 1;
      for i := msb downto 0 do
	if btest(p, i) then
	  n := (n*n*2) mod q
	else
	  n := (n*n) mod q;
      if n = 1 then
      begin
	mfactor := q;
	exit;
      end;
    end;
    mfactor := 0;
  end;

var
  exponent, factor: longint;

begin
  write('Enter the exponent of the Mersenne number (suggestion: 929): ');
  readln(exponent);
  if not isPrime(exponent) then
  begin
    writeln('M', exponent, ' (2**', exponent, ' - 1) is not prime.');
    exit;
  end;
  factor := MFactor(exponent);
  if factor = 0 then
    writeln('M', exponent, ' (2**', exponent, ' - 1) has no factor.')
  else
    writeln('M', exponent, ' (2**', exponent, ' - 1) has the factor: ', factor);
end.
