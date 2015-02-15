program dateform;
uses DOS;

{ Format digit with leading zero }
function lz(w: word): string;
var
  s: string;
begin
  str(w,s);
  if length(s) = 1 then
    s := '0' + s;
  lz := s
end;

function m2s(mon: integer): string;
begin
  case mon of
     1: m2s := 'January';
     2: m2s := 'February';
     3: m2s := 'March';
     4: m2s := 'April';
     5: m2s := 'May';
     6: m2s := 'June';
     7: m2s := 'July';
     8: m2s := 'August';
     9: m2s := 'September';
    10: m2s := 'October';
    11: m2s := 'November';
    12: m2s := 'December'
  end
end;

function d2s(dow: integer): string;
begin
  case dow of
    0: d2s := 'Sunday';
    1: d2s := 'Monday';
    2: d2s := 'Tueday';
    3: d2s := 'Wednesday';
    4: d2s := 'Thursday';
    5: d2s := 'Friday';
    6: d2s := 'Saturday'
  end
end;

var
  yr,mo,dy,dow: word;
  mname,dname: string;

begin
  GetDate(yr,mo,dy,dow);
  writeln(yr,'-',lz(mo),'-',lz(dy));
  mname := m2s(mo); dname := d2s(dow);
  writeln(dname,', ',mname,' ',dy,', ',yr)
end.
