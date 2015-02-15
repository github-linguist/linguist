program HashFromTwoArrays (Output);

uses
  contnrs;

var
  keys:   array[1..3] of string  = ('a', 'b', 'c');
  values: array[1..3] of integer = ( 1,   2,   3 );
  hash:   TFPDataHashTable;
  i:      integer;

begin
  hash := TFPDataHashTable.Create;
  for i := low(keys) to high(keys) do
    hash.add(keys[i], @values[i]);
  writeln ('Length of hash table: ', hash.Count);
  hash.Destroy;
end.
