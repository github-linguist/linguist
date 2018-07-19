Program SunDial;

Const
   pi  = 3.14159265358979323846;
   dr  = pi/180.0;
   rd  = 180.0/pi;
   tab =  chr(9);

Var
   lat, slat, lng, ref : Real;
   hla, hra	       : Real;
   h		       : Integer;

function tan(val : Real) : Real;
begin
   tan := sin(val)/cos(val)
end;

Begin
   Write('Enter latitude: '); Read(lat);
   Write('Enter longitude: '); Read(lng);
   Write('Enter legal meridian: '); Read(ref);
   WriteLn;
   slat := sin(lat * dr);
   WriteLn('sine of latitude: ', slat);
   WriteLn('diff longitude: ', lng - ref);
   WriteLn('Hour, sun hour angle, dial hour line angle from 6am to 6pm');
   for h := -6 to 6 do begin
      hra := 15.0 * h;
      hra := hra - lng + ref;
      hla := arctan(slat * tan(hra * dr)) * rd;
      WriteLn('HR= ', h:3, ';  ',
	      tab, '  HRA= ', hra:7:3, ';  ',
	      tab, '  HLA= ', hla:7:3)
   end
end.
