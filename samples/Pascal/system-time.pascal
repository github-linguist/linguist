program systime;
uses DOS;

{ Format digit with leading zero }
function lz(w: word): string;
var
  s: string;
begin
  str(w,s);
  if length(s) = 1 then
    s := '0' + s;
  lz := s;
end;

var
  h,m,s,c: word;
  yr,mo,da,dw: word;
  dt: datetime;
  t,ssm: longint;
  regs: registers;

begin

  { Time and Date }
  GetTime(h,m,s,c);
  writeln(lz(h),':',lz(m),':',lz(s),'.',c);
  GetDate(yr,mo,da,dw);
  writeln(yr,'-',lz(mo),'-',lz(da));

  { Turbo Epoch, seconds }
  with dt do begin
    year := yr; month := mo; day := da;
    hour := h; min := m; sec := s;
  end;
  packtime(dt,t);
  writeln(t);

  { Seconds since midnight, PC-BIOS 1Ah }
  regs.ah := 0; Intr($1A,regs);
  ssm := round((regs.cx * 65536 + regs.dx) * (65536 / 1192180));
  writeln(ssm);

end.
