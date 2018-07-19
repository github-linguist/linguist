declare
  PI = 3.14159265

  fun {FromDegrees Deg}
     Deg * PI / 180.
  end

  fun {ToDegrees Rad}
     Rad * 180. / PI
  end

  Radians = PI / 4.
  Degrees = 45.
in
  for F in [Sin Cos Tan] do
     {System.showInfo {F Radians}#"  "#{F {FromDegrees Degrees}}}
  end

  for I#F in [Asin#Sin Acos#Cos Atan#Tan] do
     {System.showInfo {I {F Radians}}#"  "#{ToDegrees {I {F Radians}}}}
  end
