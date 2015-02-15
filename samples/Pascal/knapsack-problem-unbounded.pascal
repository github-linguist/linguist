Program Knapsack(output);

uses
  math;

type
  bounty = record
    value: longint;
    weight, volume: real;
  end;

const
  panacea: bounty = (value:3000; weight:  0.3; volume: 0.025);
  ichor:   bounty = (value:1800; weight:  0.2; volume: 0.015);
  gold:    bounty = (value:2500; weight:  2.0; volume: 0.002);
  sack:    bounty = (value:   0; weight: 25.0; volume: 0.25);

var
  totalweight, totalvolume: real;
  maxpanacea, maxichor, maxgold: longint;
  maxvalue: longint = 0;
  n: array [1..3] of longint;
  current: bounty;
  i, j, k: longint;

begin
  maxpanacea := round(min(sack.weight / panacea.weight, sack.volume / panacea.volume));
  maxichor   := round(min(sack.weight / ichor.weight,   sack.volume / ichor.volume));
  maxgold    := round(min(sack.weight / gold.weight,    sack.volume / gold.volume));

  for i := 0 to maxpanacea do
    for j := 0 to maxichor do
      for k := 0 to maxgold do
      begin
        current.value  := k * gold.value  + j * ichor.value  + i * panacea.value;
        current.weight := k * gold.weight + j * ichor.weight + i * panacea.weight;
        current.volume := k * gold.volume + j * ichor.volume + i * panacea.volume;
        if (current.value > maxvalue)      and
	   (current.weight <= sack.weight) and
           (current.volume <= sack.volume) then
	begin
          maxvalue    := current.value;
          totalweight := current.weight;
          totalvolume := current.volume;
          n[1] := i;
	  n[2] := j;
	  n[3] := k;
        end;
      end;

  writeln ('Maximum value achievable is ', maxValue);
  writeln ('This is achieved by carrying ', n[1], ' panacea, ', n[2], ' ichor and ', n[3], ' gold items');
  writeln ('The weight of this carry is ', totalWeight:6:3, ' and the volume used is ', totalVolume:6:4);
end.
