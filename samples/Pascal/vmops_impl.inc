operator+(a, b: TVector)res: TVector;
var
  i, an, bn, rn: Integer;
begin
  an := Length(a);
  bn := Length(b);
  rn := math.max(an, bn);
  SetLength(res, rn);
  for i := 0 to rn - 1 do res[i] := a[i mod an] + b[i mod bn];
end;

operator+(a: TVector; b: Double)res: TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] + b;
end;

operator+(a: Double; b: TVector)res: TVector;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a + b[i];
end;

operator-(a: TVector)res: TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := -a[i];
end;

operator-(a, b: TVector)res: TVector;
var
  i, an, bn, rn: Integer;
begin
  an := Length(a);
  bn := Length(b);
  rn := math.max(an, bn);
  SetLength(res, rn);
  for i := 0 to rn - 1 do res[i] := a[i mod an] - b[i mod bn];
end;

operator-(a: TVector; b: Double)res: TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] - b;
end;

operator-(a: Double; b: TVector)res: TVector;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a - b[i];
end;

operator*(a, b: TVector)res: TVector;
var
  i, an, bn, rn: Integer;
begin
  an := Length(a);
  bn := Length(b);
  rn := math.max(an, bn);
  SetLength(res, rn);
  for i := 0 to rn - 1 do res[i] := a[i mod an] * b[i mod bn];
end;

operator*(a: TVector; b: Double)res: TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] * b;
end;

operator*(a: Double; b: TVector)res: TVector;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a * b[i];
end;

operator/(a, b: TVector)res: TVector;
var
  i, an, bn, rn: Integer;
begin
  an := Length(a);
  bn := Length(b);
  rn := math.max(an, bn);
  SetLength(res, rn);
  for i := 0 to rn - 1 do res[i] := a[i mod an] / b[i mod bn];
end;

operator/(a: TVector; b: Double)res: TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] / b;
end;

operator/(a: Double; b: TVector)res: TVector;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a / b[i];
end;

operator**(a, b: Double)res: Double;
begin
  if IsNan(a) or IsNan(b) then res := NaN
  else if b = Infinity then begin
    if a = NegInfinity then res := NaN
    else case sign(a) of
      1: res := Infinity;
      0: res := 0;
      -1: res := NegInfinity;
    end;
  end else if b = NegInfinity then begin
    if a = NegInfinity then res := NaN
    else case sign(a) of
      1: res := 0;
      0: res := Infinity;
      -1: res := 0;
    end;
  end else res := power(a, b);
end;

operator**(a, b: TVector)res: TVector;
var
  i, an, bn, rn: Integer;
begin
  an := Length(a);
  bn := Length(b);
  rn := math.max(an, bn);
  SetLength(res, rn);
  for i := 0 to rn - 1 do res[i] := a[i mod an] ** b[i mod bn];
end;

operator**(a: TVector; b: Double)res: TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] ** b;
end;

operator**(a: Double; b: TVector)res: TVector;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a ** b[i];
end;

operator><(a: TVector; b: TVector)res: TMatrix;
var
  i, an, bn, rn: Integer;
begin
  an := Length(a);
  bn := Length(b);
  if an * bn = 0 then Exit(nil);
  rn := math.max(an, bn);
  SetLength(res, 1);
  SetLength(res[0], 1);
  res[0][0] := 0;
  for i := 0 to rn - 1 do res[0][0] := res[0][0] + a[i mod an] * b[i mod bn];
end;

operator+(a: TMatrix; b: TMatrix)res: TMatrix;
var
  i, an: Integer;
begin
  an := Length(a);
  SetLength(res, an);
  for i := 0 to an - 1 do res[i] := a[i] + b[i];
end;

operator+(a: TMatrix; b: Double)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] + b;
end;

operator+(a: Double; b: TMatrix)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a + b[i];
end;

operator+(a: TMatrix; b: TVector)res: TMatrix;
begin
  res := a + matrix(b, Length(a), Length(a[0]));
end;

operator+(a: TVector; b: TMatrix)res: TMatrix;
begin
  res := b + matrix(a, Length(b), Length(b[0]));
end;

operator-(a: TMatrix)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := -a[i];
end;

operator-(a: TMatrix; b: TMatrix)res: TMatrix;
var
  i, an: Integer;
begin
  an := Length(a);
  SetLength(res, an);
  for i := 0 to an - 1 do res[i] := a[i] - b[i];
end;

operator-(a: TMatrix; b: Double)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] - b;
end;

operator-(a: Double; b: TMatrix)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a - b[i];
end;

operator-(a: TMatrix; b: TVector)res: TMatrix;
begin
  res := a - matrix(b, Length(a), Length(a[0]));
end;

operator-(a: TVector; b: TMatrix)res: TMatrix;
begin
  res := b - matrix(a, Length(b), Length(b[0]));
end;

operator*(a: TMatrix; b: TMatrix)res: TMatrix;
var
  i, an: Integer;
begin
  an := Length(a);
  SetLength(res, an);
  for i := 0 to an - 1 do res[i] := a[i] * b[i];
end;

operator*(a: TMatrix; b: Double)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] * b;
end;

operator*(a: Double; b: TMatrix)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a * b[i];
end;

operator*(a: TMatrix; b: TVector)res: TMatrix;
begin
  res := a * matrix(b, Length(a), Length(a[0]));
end;

operator*(a: TVector; b: TMatrix)res: TMatrix;
begin
  res := b * matrix(a, Length(b), Length(b[0]));
end;

operator/(a: TMatrix; b: TMatrix)res: TMatrix;
var
  i, an: Integer;
begin
  an := Length(a);
  SetLength(res, an);
  for i := 0 to an - 1 do res[i] := a[i] / b[i];
end;

operator/(a: TMatrix; b: Double)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] / b;
end;

operator/(a: Double; b: TMatrix)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a / b[i];
end;

operator/(a: TMatrix; b: TVector)res: TMatrix;
begin
  res := a / matrix(b, Length(a), Length(a[0]));
end;

operator/(a: TVector; b: TMatrix)res: TMatrix;
begin
  res := b / matrix(a, Length(b), Length(b[0]));
end;

operator**(a: TMatrix; b: TMatrix)res: TMatrix;
var
  i, an: Integer;
begin
  an := Length(a);
  SetLength(res, an);
  for i := 0 to an - 1 do res[i] := a[i] ** b[i];
end;

operator**(a: TMatrix; b: Double)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a[i] ** b;
end;

operator**(a: Double; b: TMatrix)res: TMatrix;
var
  i, n: Integer;
begin
  n := Length(b);
  SetLength(res, n);
  for i := 0 to n - 1 do res[i] := a ** b[i];
end;

operator**(a: TMatrix; b: TVector)res: TMatrix;
begin
  res := a ** matrix(b, Length(a), Length(a[0]));
end;

operator**(a: TVector; b: TMatrix)res: TMatrix;
begin
  res := b ** matrix(a, Length(b), Length(b[0]));
end;

operator><(a: TMatrix; b: TMatrix)res: TMatrix;
var
  i, j, n, m: Integer;
begin
  n := Length(a);
  m := Length(b);
  if (n = 1) and (m = 1) then Exit(vector(a) >< vector(b));
  SetLength(res, n);
  for i := 0 to n - 1 do begin
    j := 0;
    res[i] := a[i][j] * b[j];
    for j := 1 to m - 1 do res[i] := res[i] + a[i][j] * b[j];
  end;
end;

operator><(a: TMatrix; b: TVector)res: TMatrix;
begin
  res := a >< matrix(b, 1, Length(b));
end;

operator><(a: TVector; b: TMatrix)res: TMatrix;
begin
  res := matrix(a, 1, Length(a)) >< b;
end;

function vector(a: array of const): TVector;
var
  i, n: Integer;
begin
  n := Length(a);
  SetLength(Result, n);
  for i := 0 to n - 1 do case a[i].VType of
    vtInteger:  Result[i] := a[i].VInteger;
    vtExtended: Result[i] := a[i].VExtended^;
    vtCurrency: Result[i] := a[i].VCurrency^;
    vtInt64:    Result[i] := a[i].VInt64^;
  end;
end;

function vector(a: TMatrix): TVector; inline;
var
  i, j, n, m : Integer;
begin
  n := Length(a);
  if n = 0 then Exit(nil);
  m := Length(a[0]);
  if m = 0 then Exit(nil);
  SetLength(Result, m * n);
  for i := 0 to m - 1 do
    for j := 0 to n - 1 do
      Result[i * n + j] := a[j][i];
end;

function matrix(data: TVector; nrow, ncol: Integer; byrow: Boolean): TMatrix;
var
  i, j, n, c: Integer;
begin
  n := Length(data);
  if n = 0 then Exit(nil);
  if (nrow = 0) and (ncol = 0) then ncol := 1;
  if nrow = 0 then nrow := (n div ncol) + sign(n mod ncol);
  if ncol = 0 then ncol := (n div nrow) + sign(n mod nrow);
  SetLength(Result, nrow);
  for i := 0 to nrow - 1 do begin
    SetLength(Result[i], ncol);
    for j := 0 to ncol - 1 do begin
      if byrow then c := i * ncol + j else c := j * nrow + i;
      Result[i][j] := data[c mod n];
    end;
  end;
end;

function matrix(data: TMatrix; nrow, ncol: Integer; byrow: Boolean): TMatrix;
begin
  Result := matrix(vector(data), nrow, ncol, byrow);
end;

function rep(data: TVector; times: TVector): TVector;
var
  i, j, n, m, s, c: Integer;
begin
  Result := nil;
  n := Length(data);
  m := Length(times);
  if n * m = 0 then Exit;
  s := 0;
  for i := 0 to n - 1 do begin
    c := trunc(times[i mod m]);
    if c > 0 then s += c;
  end;
  SetLength(Result, s);
  c := 0;
  for i := 0 to n - 1 do begin
    s := trunc(times[i mod m]);
    for j := 1 to s do begin
      Result[c] := data[i];
      Inc(c);
    end;
  end;
end;

function rep(data: TVector; len: Integer): TVector;
var
  i, j, n: Integer;
begin
  n := Length(data);
  SetLength(Result, n * len);
  if n = 0 then Exit;
  for i := 0 to len - 1 do
    for j := 0 to n - 1 do
      Result[i * n + j] := data[j];
end;

function rep(data: Double; len: Integer): TVector;
var
  i: Integer;
begin
  SetLength(Result, len);
  for i := 0 to len - 1 do Result[i] := data;
end;
